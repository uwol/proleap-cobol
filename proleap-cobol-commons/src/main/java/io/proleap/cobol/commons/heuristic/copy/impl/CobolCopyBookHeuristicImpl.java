package io.proleap.cobol.commons.heuristic.copy.impl;

import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jakarta.inject.Singleton;

import io.proleap.cobol.commons.heuristic.copy.CobolCopyBookHeuristic;

@Singleton
public class CobolCopyBookHeuristicImpl implements CobolCopyBookHeuristic {

	@Override
	public boolean determineIsCopyBook(final String cobolText) {
		final Pattern variableIdentificationPattern = Pattern.compile(CobolCopyBookHeuristic.VARIABLE_IDENTIFICATION,
				Pattern.CASE_INSENSITIVE);

		final Pattern tandemIdentificationPattern = Pattern.compile(CobolCopyBookHeuristic.TANDEM_IDENTIFICATION,
				Pattern.CASE_INSENSITIVE);

		final Scanner scanner = new Scanner(cobolText);
		boolean result = true;

		while (scanner.hasNextLine()) {
			final String line = scanner.nextLine();

			final Matcher variableIdentificationMatcher = variableIdentificationPattern.matcher(line);
			final Matcher tandemIdentificationMatcher = tandemIdentificationPattern.matcher(line);

			if (variableIdentificationMatcher.matches()) {
				result = false;
			} else if (tandemIdentificationMatcher.matches()) {
				result = false;
			}
		}

		scanner.close();

		return result;
	}
}
