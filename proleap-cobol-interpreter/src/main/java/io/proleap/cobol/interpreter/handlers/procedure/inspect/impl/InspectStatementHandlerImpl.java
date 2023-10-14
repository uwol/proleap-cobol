package io.proleap.cobol.interpreter.handlers.procedure.inspect.impl;

import java.math.BigDecimal;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.inspect.AllLeading;
import io.proleap.cobol.asg.metamodel.procedure.inspect.AllLeadingPhrase;
import io.proleap.cobol.asg.metamodel.procedure.inspect.AllLeadingPhrase.AllLeadingsType;
import io.proleap.cobol.asg.metamodel.procedure.inspect.BeforeAfterPhrase;
import io.proleap.cobol.asg.metamodel.procedure.inspect.BeforeAfterPhrase.BeforeAfterType;
import io.proleap.cobol.asg.metamodel.procedure.inspect.By;
import io.proleap.cobol.asg.metamodel.procedure.inspect.Characters;
import io.proleap.cobol.asg.metamodel.procedure.inspect.Converting;
import io.proleap.cobol.asg.metamodel.procedure.inspect.For;
import io.proleap.cobol.asg.metamodel.procedure.inspect.InspectStatement;
import io.proleap.cobol.asg.metamodel.procedure.inspect.InspectStatement.InspectType;
import io.proleap.cobol.asg.metamodel.procedure.inspect.Replacing;
import io.proleap.cobol.asg.metamodel.procedure.inspect.ReplacingAllLeading;
import io.proleap.cobol.asg.metamodel.procedure.inspect.ReplacingAllLeadings;
import io.proleap.cobol.asg.metamodel.procedure.inspect.ReplacingAllLeadings.ReplacingAllLeadingsType;
import io.proleap.cobol.asg.metamodel.procedure.inspect.ReplacingCharacters;
import io.proleap.cobol.asg.metamodel.procedure.inspect.Tallying;
import io.proleap.cobol.asg.metamodel.procedure.inspect.TallyingReplacing;
import io.proleap.cobol.asg.metamodel.procedure.inspect.To;
import io.proleap.cobol.asg.util.StringUtils;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.impl.CobolDecimalValueImpl;
import io.proleap.cobol.commons.value.domain.impl.CobolStringValueImpl;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.inspect.InspectStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class InspectStatementHandlerImpl extends StatementHandlerImpl<InspectStatement>
		implements InspectStatementHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolStorageService storageService;

	@Inject
	private CobolValueService valueService;

	@Inject
	private CobolValueStmtService valueStmtService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	private int countLeading(final String text, final char searchedChar) {
		int count = 0;

		for (final char c : text.toCharArray()) {
			if (searchedChar != c) {
				break;
			} else {
				count++;
			}
		}

		return count;
	}

	private int countLeadingOrAll(final String haystack, final char needle, final AllLeadingsType allLeadingsType) {
		switch (allLeadingsType) {
		case LEADING:
			return countLeading(haystack, needle);
		case ALL:
		default:
			return StringUtils.countMatches(haystack, String.valueOf(needle));
		}
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.INSPECT;
	}

	private String replaceAfter(final String text, final String searchedString, final char replacement) {
		final int searchedCharacterPosition = text.indexOf(searchedString);

		return text.substring(0, searchedCharacterPosition + 1)
				.concat(String.valueOf(replacement).repeat(text.length() - searchedCharacterPosition - 1));
	}

	private String replaceBefore(final String text, final String searchedString, final char replacement) {
		final int searchedCharacterPosition = text.indexOf(searchedString);

		return String.valueOf(replacement).repeat(text.length() - searchedCharacterPosition)
				.concat(text.substring(searchedCharacterPosition, text.length()));
	}

	private String replaceCharwise(final String input, final String fromString, final String toString,
			final CobolInterpreterParams params) {
		final char[] fromCharArray = fromString.toCharArray();
		final char[] toCharArray = toString.toCharArray();
		String text = input;

		for (int i = 0; i < fromCharArray.length; i++) {
			final char fromChar = fromCharArray[i];
			final char toChar = toCharArray[i];

			text = text.replace(fromChar, toChar);
		}

		return text;
	}

	private String replaceLeading(final String text, final char searchedChar, final char replacement) {
		final char[] textCharArray = text.toCharArray();

		for (int i = 0; i < textCharArray.length; i++) {
			final char c = textCharArray[i];

			if (searchedChar != c) {
				break;
			} else {
				textCharArray[i] = replacement;
			}
		}

		return String.valueOf(textCharArray);
	}

	protected void run(final DataDescriptionEntry dataItem, final Converting converting,
			final CobolInterpreterParams params) {
		final String fromString = valueService
				.getString(valueStmtService.getValue(converting.getFromValueStmt(), params.getState().getStorage()));

		final To to = converting.getTo();
		final String toString = valueService
				.getString(valueStmtService.getValue(to.getToValueStmt(), params.getState().getStorage()));

		final CobolValue value = storageService.getValue(dataItem, params.getState().getStorage());
		final String valueString = valueService.getString(value);
		final String result = replaceCharwise(valueString, fromString, toString, params);

		storageService.putValue(dataItem, CobolStringValueImpl.of(result), params.getState().getStorage());
	}

	protected void run(final DataDescriptionEntry dataItem, final Replacing replacing,
			final CobolInterpreterParams params) {
		final CobolValue value = storageService.getValue(dataItem, params.getState().getStorage());
		String result = valueService.getString(value);

		for (final ReplacingCharacters replacingCharacters : replacing.getCharacters()) {
			final By by = replacingCharacters.getBy();
			final String replacementString = valueService
					.getString(valueStmtService.getValue(by.getByValueStmt(), params.getState().getStorage()));
			final char replacementChar = replacementString.charAt(0);

			if (replacingCharacters.getBeforeAfterPhrases().isEmpty()) {
				result = String.valueOf(replacementChar).repeat(result.length());
			} else {
				for (final BeforeAfterPhrase beforeAfterPhrase : replacingCharacters.getBeforeAfterPhrases()) {
					final String split = valueService.getString(valueStmtService
							.getValue(beforeAfterPhrase.getDataItemValueStmt(), params.getState().getStorage()));
					final BeforeAfterType beforeAfterType = beforeAfterPhrase.getBeforeAfterType();

					switch (beforeAfterType) {
					case AFTER:
						result = replaceAfter(result, split, replacementChar);
						break;
					case BEFORE:
						result = replaceBefore(result, split, replacementChar);
						break;
					}
				}
			}
		}

		for (final ReplacingAllLeadings replacingAllLeadings : replacing.getAllLeadings()) {
			final ReplacingAllLeadingsType replacingAllLeadingsType = replacingAllLeadings
					.getReplacingAllLeadingsType();

			for (final ReplacingAllLeading replacingAllLeading : replacingAllLeadings.getAllLeadings()) {
				final By by = replacingAllLeading.getBy();
				final String replacementString = valueService
						.getString(valueStmtService.getValue(by.getByValueStmt(), params.getState().getStorage()));
				final char replacementChar = replacementString.charAt(0);

				final String patternString = valueService.getString(valueStmtService
						.getValue(replacingAllLeading.getPatternDataItemValueStmt(), params.getState().getStorage()));
				final char patternChar = patternString.charAt(0);

				switch (replacingAllLeadingsType) {
				case ALL:
					result = result.replace(patternString, replacementString);
					break;
				case FIRST:
					result = result.replaceFirst(Pattern.quote(patternString),
							Matcher.quoteReplacement(replacementString));
					break;
				case LEADING:
					result = replaceLeading(result, patternChar, replacementChar);
					break;
				}
			}
		}

		storageService.putValue(dataItem, CobolStringValueImpl.of(result), params.getState().getStorage());
	}

	protected void run(final DataDescriptionEntry dataItem, final Tallying tallying,
			final CobolInterpreterParams params) {
		for (final For tallyingFor : tallying.getFors()) {
			runFor(dataItem, tallyingFor, params);
		}
	}

	protected void run(final DataDescriptionEntry dataItem, final TallyingReplacing tallyingReplacing,
			final CobolInterpreterParams params) {
		for (final For tallyingFor : tallyingReplacing.getFors()) {
			runFor(dataItem, tallyingFor, params);
		}

		for (final Replacing replacing : tallyingReplacing.getReplacings()) {
			run(dataItem, replacing, params);
		}
	}

	@Override
	public void run(final InspectStatement statement, final CobolInterpreterParams params) {
		final DataDescriptionEntry dataItem = dataDescriptionEntryService
				.getDataDescriptionEntry(statement.getDataItemCall());
		final InspectType inspectType = statement.getInspectType();

		switch (inspectType) {
		case CONVERTING:
			final Converting converting = statement.getConverting();
			run(dataItem, converting, params);
			break;
		case REPLACING:
			final Replacing replacing = statement.getReplacing();
			run(dataItem, replacing, params);
			break;
		case TALLYING:
			final Tallying tallying = statement.getTallying();
			run(dataItem, tallying, params);
			break;
		case TALLYING_REPLACING:
			final TallyingReplacing tallyingReplacing = statement.getTallyingReplacing();
			run(dataItem, tallyingReplacing, params);
			break;
		default:
		}
	}

	protected void runFor(final DataDescriptionEntry dataItem, final For tallyingFor,
			final CobolInterpreterParams params) {
		final String haystack = valueService
				.getString(storageService.getValue(dataItem, params.getState().getStorage()));
		int totalCount = 0;

		for (final AllLeadingPhrase allLeadingPhrase : tallyingFor.getAllLeadingPhrase()) {
			final AllLeadingsType allLeadingsType = allLeadingPhrase.getAllLeadingsType();

			for (final AllLeading allLeading : allLeadingPhrase.getAllLeadings()) {
				final String needleString = valueService.getString(valueStmtService
						.getValue(allLeading.getPatternDataItemValueStmt(), params.getState().getStorage()));
				final char needle = needleString.charAt(0);
				int allLeadingCount = 0;

				if (allLeading.getBeforeAfterPhrases().isEmpty()) {
					allLeadingCount = countLeadingOrAll(haystack, needle, allLeadingsType);
				} else {
					for (final BeforeAfterPhrase beforeAfterPhrase : allLeading.getBeforeAfterPhrases()) {
						final String split = valueService.getString(valueStmtService
								.getValue(beforeAfterPhrase.getDataItemValueStmt(), params.getState().getStorage()));
						final BeforeAfterType beforeAfterType = beforeAfterPhrase.getBeforeAfterType();
						final int beforeAfterPhraseCount;

						switch (beforeAfterType) {
						case AFTER:
							final String haystackAfter = StringUtils.substringAfter(haystack, split);
							beforeAfterPhraseCount = countLeadingOrAll(haystackAfter, needle, allLeadingsType);
							break;
						case BEFORE:
						default:
							final String haystackBefore = StringUtils.substringBefore(haystack, split);
							beforeAfterPhraseCount = countLeadingOrAll(haystackBefore, needle, allLeadingsType);
							break;
						}

						allLeadingCount += beforeAfterPhraseCount;
					}
				}

				totalCount += allLeadingCount;
			}
		}

		for (final Characters characters : tallyingFor.getCharacters()) {
			int charactersResult = 0;

			if (characters.getBeforeAfterPhrases().isEmpty()) {
				charactersResult = haystack.length();
			} else {
				for (final BeforeAfterPhrase beforeAfterPhrase : characters.getBeforeAfterPhrases()) {
					final String split = valueService.getString(valueStmtService
							.getValue(beforeAfterPhrase.getDataItemValueStmt(), params.getState().getStorage()));
					final BeforeAfterType beforeAfterType = beforeAfterPhrase.getBeforeAfterType();
					final int beforeAfterPhraseCount;

					switch (beforeAfterType) {
					case AFTER:
						final String haystackAfter = StringUtils.substringAfter(haystack, split);
						beforeAfterPhraseCount = haystackAfter.length();
						break;
					case BEFORE:
					default:
						final String haystackBefore = StringUtils.substringBefore(haystack, split);
						beforeAfterPhraseCount = haystackBefore.length();
						break;
					}

					charactersResult += beforeAfterPhraseCount;
				}
			}

			totalCount += charactersResult;
		}

		final DataDescriptionEntry tallyCountDataDescriptionEntry = dataDescriptionEntryService
				.getDataDescriptionEntry(tallyingFor.getTallyCountDataItemCall());
		storageService.putValue(tallyCountDataDescriptionEntry,
				CobolDecimalValueImpl.of(BigDecimal.valueOf(totalCount)), params.getState().getStorage());
	}
}
