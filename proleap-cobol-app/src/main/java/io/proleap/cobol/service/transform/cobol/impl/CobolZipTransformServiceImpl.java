package io.proleap.cobol.service.transform.cobol.impl;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.List;
import java.util.zip.ZipOutputStream;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Strings;

import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.log.ProLeapThreadLocalLogAppender;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.service.impl.AbstractCobolService;
import io.proleap.cobol.service.input.RequestCloneFilesService;
import io.proleap.cobol.service.output.ZipOutputFileService;
import io.proleap.cobol.service.transform.cobol.CobolZipTransformService;
import io.proleap.cobol.transform.java.runner.CobolTransformationRunner;
import io.proleap.cobol.util.FilesUtils;
import io.proleap.cobol.util.RequestUtils;

@Singleton
public class CobolZipTransformServiceImpl extends AbstractCobolService implements CobolZipTransformService {

	private static final String JAVA_PACKAGE = "";

	private final static Logger LOG = LoggerFactory.getLogger(CobolTextTransformServiceImpl.class);

	private static final String POM_XML = "pom.xml";

	private static final String TRANSFORM_LOG = "transform.log";

	@Inject
	private CobolTransformationRunner cobolTransformationRunner;

	@Inject
	private RequestCloneFilesService requestCloneFilesService;

	@Inject
	private ZipOutputFileService zipOutputFileService;

	protected void doTransform(final List<File> inputFiles, final List<File> copyBookFiles,
			final String transformationId, final HttpServletResponse resp) {
		List<File> transformedFiles = null;
		File pomXmlFile = null;
		ZipOutputStream zipOutputStream = null;

		try {
			zipOutputStream = new ZipOutputStream(resp.getOutputStream());

			for (final File inputFile : inputFiles) {
				final CobolSourceFormatEnum lineFormat = determineLineFormat(inputFile);
				final CobolParserParams params = createParserParams(copyBookFiles);
				params.setFormat(lineFormat);

				transformedFiles = cobolTransformationRunner.transformFile(inputFile, JAVA_PACKAGE, params);

				for (final File transformedFile : transformedFiles) {
					zipOutputFileService.addTransformationClassZipEntry(transformedFile, zipOutputStream);
				}

				FilesUtils.deleteFiles(transformedFiles);

				System.gc();
			}

			pomXmlFile = getPomXmlFile();

			zipOutputFileService.addZipEntry(pomXmlFile, "", zipOutputStream);
			zipOutputFileService.addLogZipEntry(ProLeapThreadLocalLogAppender.getLog(), TRANSFORM_LOG, zipOutputStream);

			zipOutputStream.flush();
			zipOutputStream.close();
		} catch (final Exception e) {
			handleException(resp, e);
		} finally {
			FilesUtils.deleteFiles(transformedFiles);
			FilesUtils.deleteFile(pomXmlFile);
		}
	}

	protected File getPomXmlFile() throws IOException {
		final InputStream resourceAsStream = getClass().getResourceAsStream("/io/proleap/jar/cobol/pom.xml");
		final File result = FilesUtils.createTempFile(POM_XML);

		Files.copy(resourceAsStream, result.toPath());

		return result;
	}

	@Override
	public void transform(final HttpServletRequest req, final HttpServletResponse resp) {
		List<File> inputFiles = null;
		List<File> copyBookFiles = null;
		File tempDir = null;

		try {
			final String authToken = RequestUtils.fetchAuthToken(req);
			final String projectId = RequestUtils.fetchProjectId(req);
			final String transformationId = RequestUtils.fetchTransformationId(req);

			if (Strings.isNullOrEmpty(authToken)) {
				LOG.warn("Will not log to remote api as auth token is {}.", authToken);
			}

			if (Strings.isNullOrEmpty(projectId)) {
				LOG.warn("Will not log to remote api as project id is {}.", projectId);
			}

			if (Strings.isNullOrEmpty(transformationId)) {
				LOG.warn("Will not log to remote api as transformation id is {}.", transformationId);
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
					doTransform(inputFiles, copyBookFiles, transformationId, resp);
				}
			}
		} catch (final Exception e) {
			handleException(resp, e);
		} finally {
			ProLeapThreadLocalLogAppender.reset();

			FilesUtils.deleteFiles(inputFiles);
			FilesUtils.deleteFiles(copyBookFiles);
			FilesUtils.deleteFile(tempDir);
		}
	}
}
