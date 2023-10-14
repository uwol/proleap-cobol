package io.proleap.cobol.commons.heuristic.lineformat.impl;

import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jakarta.inject.Singleton;

import io.proleap.cobol.commons.heuristic.lineformat.CobolLineFormatHeuristic;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@Singleton
public class CobolLineFormatHeuristicImpl implements CobolLineFormatHeuristic {

	@Override
	public CobolSourceFormatEnum determineLineFormat(final String cobolText) {
		// fixed
		final Pattern fixedPattern = Pattern.compile(CobolLineFormatHeuristic.SOURCE_FORMAT_FIXED,
				Pattern.CASE_INSENSITIVE);

		// variable
		final Pattern variableIdentificationPattern = Pattern
				.compile(CobolLineFormatHeuristic.SOURCE_FORMAT_VARIABLE_IDENTIFICATION, Pattern.CASE_INSENSITIVE);

		// tandem
		final Pattern tandemIdentificationPattern = Pattern
				.compile(CobolLineFormatHeuristic.SOURCE_FORMAT_TANDEM_IDENTIFICATION, Pattern.CASE_INSENSITIVE);

		final Scanner scanner = new Scanner(cobolText);
		CobolSourceFormatEnum result = CobolSourceFormatEnum.FIXED;

		while (scanner.hasNextLine()) {
			final String line = scanner.nextLine();

			final Matcher fixedMatcher = fixedPattern.matcher(line);
			final Matcher variableIdentificationMatcher = variableIdentificationPattern.matcher(line);
			final Matcher tandemIdentificationMatcher = tandemIdentificationPattern.matcher(line);

			if (variableIdentificationMatcher.matches() && fixedMatcher.matches()) {
				result = CobolSourceFormatEnum.FIXED;
			} else if (variableIdentificationMatcher.matches()) {
				result = CobolSourceFormatEnum.VARIABLE;
			} else if (tandemIdentificationMatcher.matches()) {
				result = CobolSourceFormatEnum.TANDEM;
			}
		}

		scanner.close();

		return result;
	}
}
