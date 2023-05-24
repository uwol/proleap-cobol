package io.proleap.cobol.service.analysis.cobol.impl;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Collections;
import java.util.List;
import java.util.zip.ZipOutputStream;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.dom4j.Document;
import org.dom4j.io.HTMLWriter;
import org.dom4j.io.OutputFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Strings;

import io.proleap.cobol.analysis.codexml.CobolCodeXmlRunner;
import io.proleap.cobol.analysis.issues.CobolIssuesRunner;
import io.proleap.cobol.analysis.issues.dto.IssuesDto;
import io.proleap.cobol.analysis.registry.CobolIdRegistry;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.log.ProLeapThreadLocalLogAppender;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.service.analysis.cobol.CobolZipAnalysisService;
import io.proleap.cobol.service.impl.AbstractCobolService;
import io.proleap.cobol.service.input.RequestCloneFilesService;
import io.proleap.cobol.service.output.ZipOutputFileService;
import io.proleap.cobol.util.FilesUtils;
import io.proleap.cobol.util.RequestUtils;

@Singleton
public class CobolZipAnalysisServiceImpl extends AbstractCobolService implements CobolZipAnalysisService {

	private static final String ANALYSIS_LOG = "analysis.log";

	private final static Logger LOG = LoggerFactory.getLogger(CobolZipAnalysisServiceImpl.class);

	@Inject
	private CobolCodeXmlRunner cobolCodeXmlRunner;

	@Inject
	private CobolIssuesRunner cobolIssuesRunner;

	@Inject
	private RequestCloneFilesService requestCloneFilesService;

	@Inject
	private ZipOutputFileService zipOutputFileService;

	@Override
	public void analyze(final HttpServletRequest req, final HttpServletResponse res) {
		List<File> inputFiles = null;
		List<File> copyBookFiles = null;
		File tempDir = null;

		try {
			final String authToken = RequestUtils.fetchAuthToken(req);
			final String projectId = RequestUtils.fetchProjectId(req);
			final String analysisId = RequestUtils.fetchAnalysisId(req);

			if (Strings.isNullOrEmpty(authToken)) {
				LOG.warn("Will not log to remote api as auth token is {}.", authToken);
			}

			if (Strings.isNullOrEmpty(projectId)) {
				LOG.warn("Will not log to remote api as project id is {}.", projectId);
			}

			if (Strings.isNullOrEmpty(analysisId)) {
				LOG.warn("Will not log to remote api as analysis id is {}.", analysisId);
			}

			ProLeapThreadLocalLogAppender.reset();

			tempDir = requestCloneFilesService.cloneFiles(req);

			if (tempDir == null) {
				LOG.warn("Could not generate temp dir from request");
			} else {
				copyBookFiles = collectCopyBookFiles(tempDir);
				inputFiles = collectInputFiles(tempDir, copyBookFiles);

				if (inputFiles == null) {
					LOG.info("Could not extract files from request");
				} else {
					doAnalyzeToZip(inputFiles, copyBookFiles, analysisId, res);
				}
			}
		} catch (final Exception e) {
			handleException(res, e);
		} finally {
			ProLeapThreadLocalLogAppender.reset();

			FilesUtils.deleteFiles(inputFiles);
			FilesUtils.deleteFiles(copyBookFiles);
			FilesUtils.deleteFile(tempDir);
		}
	}

	protected File doAnalyzeCodeViewToFileForZip(final CompilationUnit compilationUnit,
			final CobolIdRegistry idRegistry) throws IOException {
		final File result = new File(Files.createTempDirectory("tmp").toFile().getAbsolutePath(),
				compilationUnit.getName());
		final FileOutputStream fileOutputStream = new FileOutputStream(result);
		final Document document = cobolCodeXmlRunner.analyzeCompilationUnit(compilationUnit, idRegistry);

		final OutputFormat outputFormat = new OutputFormat();
		outputFormat.setTrimText(false);
		outputFormat.setNewlines(true);

		final HTMLWriter writer = new HTMLWriter(fileOutputStream, outputFormat);
		writer.write(document);
		writer.flush();
		writer.close();

		fileOutputStream.flush();
		fileOutputStream.close();

		return result;
	}

	protected void doAnalyzeToZip(final List<File> inputFiles, final List<File> copyBookFiles, final String analysisId,
			final HttpServletResponse res) {
		File currentCodeViewFile = null;
		ZipOutputStream zipOutputStream = null;

		try {
			zipOutputStream = new ZipOutputStream(res.getOutputStream());

			/**
			 * Input files are analyzed separately, as COBOL files do not depend on each
			 * other, and thus we can save heap space.
			 */
			long compilationUnitNumber = 0;
			final IssuesDto issuesDto = new IssuesDto();

			for (final File inputFile : inputFiles) {
				final CobolSourceFormatEnum lineFormat = determineLineFormat(inputFile);
				final CobolParserParams params = createParserParams(copyBookFiles);
				params.setFormat(lineFormat);

				final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, params);
				final CompilationUnit compilationUnit = program.getCompilationUnits().get(0);
				final CobolIdRegistry idRegistry = new CobolIdRegistry();

				currentCodeViewFile = doAnalyzeCodeViewToFileForZip(compilationUnit, idRegistry);
				zipOutputFileService.addAnalysisCodeViewZipEntry(currentCodeViewFile, compilationUnitNumber,
						zipOutputStream);
				FilesUtils.deleteFile(currentCodeViewFile);

				final IssuesDto compilationUnitIssuesDto = cobolIssuesRunner.analyzeCompilationUnit(compilationUnit,
						idRegistry);
				issuesDto.issues.addAll(compilationUnitIssuesDto.issues);

				compilationUnitNumber++;
				System.gc();
			}

			Collections.sort(issuesDto.issues);

			zipOutputFileService.addAnalysisIssueZipEntry(issuesDto, zipOutputStream);
			zipOutputFileService.addLogZipEntry(ProLeapThreadLocalLogAppender.getLog(), ANALYSIS_LOG, zipOutputStream);

			zipOutputStream.flush();
			zipOutputStream.close();
		} catch (final Exception e) {
			handleException(res, e);
		} finally {
			FilesUtils.deleteFile(currentCodeViewFile);
		}
	}
}
