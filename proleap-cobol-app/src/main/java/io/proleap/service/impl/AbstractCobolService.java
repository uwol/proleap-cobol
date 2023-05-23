package io.proleap.service.impl;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;
import javax.servlet.http.HttpServletResponse;

import com.google.common.base.Charsets;
import com.google.common.base.Strings;

import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.params.impl.CobolParserParamsImpl;
import io.proleap.cobol.commons.heuristic.copy.CobolCopyBookHeuristic;
import io.proleap.cobol.commons.heuristic.lineformat.CobolLineFormatHeuristic;
import io.proleap.cobol.interpreter.domain.state.impl.CobolStateImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.params.impl.CobolInterpreterParamsImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.service.mail.DevOpsMailService;

public abstract class AbstractCobolService {

	@Inject
	private CobolCopyBookHeuristic cobolCopyBookHeuristic;

	@Inject
	private CobolLineFormatHeuristic cobolLineFormatHeuristic;

	@Inject
	private DevOpsMailService devOpsMailService;

	protected List<File> collectCopyBookFiles(final File directory) throws IOException {
		final List<File> result = new ArrayList<File>();

		for (final Path path : Files.walk(directory.toPath()).collect(Collectors.toList())) {
			final String cobolText = Files.readString(path, Charsets.UTF_8);
			final boolean isCopyBookFile = cobolCopyBookHeuristic.determineIsCopyBook(cobolText);

			if (isCopyBookFile) {
				result.add(path.toFile());
			}
		}

		return result;
	}

	protected List<File> collectInputFiles(final File directory, final List<File> copyBookFiles) throws IOException {
		final List<File> allFiles = Files.walk(directory.toPath()).map(Path::toFile).collect(Collectors.toList());
		allFiles.removeAll(copyBookFiles);
		return allFiles;
	}

	protected CobolInterpreterParams createInterpreterParams(final CobolParserParams parserParams) {
		final CobolInterpreterParams result = new CobolInterpreterParamsImpl();
		result.setParserParams(parserParams);
		result.setState(new CobolStateImpl());
		return result;
	}

	protected CobolParserParams createParserParams() {
		final CobolParserParams result = new CobolParserParamsImpl();
		result.setIgnoreSyntaxErrors(false);
		return result;
	}

	protected CobolParserParams createParserParams(final List<File> copyBookFiles) {
		final CobolParserParams result = createParserParams();
		result.setCopyBookFiles(copyBookFiles);
		return result;
	}

	protected CobolSourceFormatEnum determineLineFormat(final File file) throws IOException {
		final String cobolText = Files.readString(file.toPath(), Charsets.UTF_8);
		return cobolLineFormatHeuristic.determineLineFormat(cobolText);
	}

	protected void handleException(final HttpServletResponse res, final Exception e) {
		try {
			res.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);

			final String message = e.getMessage();

			if (!Strings.isNullOrEmpty(message)) {
				res.getOutputStream().write(message.getBytes(StandardCharsets.UTF_8));
			}

			devOpsMailService.sendExceptionMessage(e);
		} catch (final IOException e1) {
		}
	}
}
