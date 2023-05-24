package io.proleap.cobol.service.transform.cobol.impl;

import java.io.File;
import java.nio.file.Files;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.service.impl.AbstractCobolService;
import io.proleap.cobol.service.transform.cobol.CobolTextTransformService;
import io.proleap.cobol.service.transform.cobol.dto.CobolTransformParamDto;
import io.proleap.cobol.transform.java.runner.CobolTransformationRunner;
import io.proleap.cobol.util.FilesUtils;

@Singleton
public class CobolTextTransformServiceImpl extends AbstractCobolService implements CobolTextTransformService {

	private static final String JAVA_PACKAGE = "";

	@Inject
	private CobolTransformationRunner cobolTransformationRunner;

	@Override
	public void transform(final HttpServletRequest req, final HttpServletResponse resp) {
		File outputFile = null;

		try {
			final CobolTransformParamDto param = new ObjectMapper().readValue(req.getInputStream(),
					CobolTransformParamDto.class);
			final CobolSourceFormatEnum lineFormat = CobolSourceFormatEnum.valueOf(param.format);

			final CobolParserParams params = createParserParams();
			params.setFormat(lineFormat);

			outputFile = cobolTransformationRunner.transformCode(param.code, "example", JAVA_PACKAGE, params).get(0);

			Files.copy(outputFile.toPath(), resp.getOutputStream());
		} catch (final Exception e) {
			handleException(resp, e);
		} finally {
			FilesUtils.deleteFile(outputFile);
		}
	}
}
