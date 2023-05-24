package io.proleap.cobol.service.analysis.cobol.impl;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Collections;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.dom4j.Document;
import org.dom4j.io.HTMLWriter;
import org.dom4j.io.OutputFormat;

import com.fasterxml.jackson.databind.ObjectMapper;

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
import io.proleap.cobol.service.analysis.cobol.CobolTextAnalysisService;
import io.proleap.cobol.service.analysis.cobol.dto.CobolAnalysisParamDto;
import io.proleap.cobol.service.analysis.cobol.dto.CobolAnalysisResultDto;
import io.proleap.cobol.service.impl.AbstractCobolService;

@Singleton
public class CobolTextAnalysisServiceImpl extends AbstractCobolService implements CobolTextAnalysisService {

	@Inject
	private CobolCodeXmlRunner cobolCodeXmlRunner;

	@Inject
	private CobolIssuesRunner cobolIssuesRunner;

	@Override
	public void analyze(final HttpServletRequest req, final HttpServletResponse resp) {
		final CobolAnalysisResultDto result = new CobolAnalysisResultDto();

		try {
			ProLeapThreadLocalLogAppender.reset();

			final CobolAnalysisParamDto param = new ObjectMapper().readValue(req.getInputStream(),
					CobolAnalysisParamDto.class);

			final CobolSourceFormatEnum lineFormat = CobolSourceFormatEnum.valueOf(param.format);

			final CobolParserParams params = createParserParams();
			params.setFormat(lineFormat);

			final Program program = new CobolParserRunnerImpl().analyzeCode(param.code, "example", params);
			final CompilationUnit compilationUnit = program.getCompilationUnits().get(0);
			final CobolIdRegistry idRegistry = new CobolIdRegistry();

			{
				result.codeView = analyzeCodeView(compilationUnit, idRegistry);

				final IssuesDto compilationUnitIssuesDto = cobolIssuesRunner.analyzeCompilationUnit(compilationUnit,
						idRegistry);
				result.issues.addAll(compilationUnitIssuesDto.issues);
			}

			Collections.sort(result.issues);

			new ObjectMapper().writeValue(resp.getOutputStream(), result);
		} catch (final Exception e) {
			handleException(resp, e);
		} finally {
			ProLeapThreadLocalLogAppender.reset();
		}
	}

	private String analyzeCodeView(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry)
			throws IOException {
		final Document document = cobolCodeXmlRunner.analyzeCompilationUnit(compilationUnit, idRegistry);

		final OutputFormat outputFormat = new OutputFormat();
		outputFormat.setTrimText(false);
		outputFormat.setNewlines(true);

		final StringWriter stringWriter = new StringWriter();
		final HTMLWriter htmlWriter = new HTMLWriter(stringWriter, outputFormat);
		htmlWriter.write(document);
		htmlWriter.flush();
		htmlWriter.close();

		return stringWriter.toString();
	}
}
