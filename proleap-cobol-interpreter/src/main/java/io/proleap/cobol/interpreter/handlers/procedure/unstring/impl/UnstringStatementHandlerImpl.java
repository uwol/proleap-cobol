package io.proleap.cobol.interpreter.handlers.procedure.unstring.impl;

import java.util.List;
import java.util.regex.Pattern;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.NotOnOverflowPhrase;
import io.proleap.cobol.asg.metamodel.procedure.OnOverflowPhrase;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.unstring.DelimitedByPhrase;
import io.proleap.cobol.asg.metamodel.procedure.unstring.Into;
import io.proleap.cobol.asg.metamodel.procedure.unstring.IntoPhrase;
import io.proleap.cobol.asg.metamodel.procedure.unstring.Sending;
import io.proleap.cobol.asg.metamodel.procedure.unstring.UnstringStatement;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.datadescription.CobolPictureLengthService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.impl.CobolStringValueImpl;
import io.proleap.cobol.interpreter.handlers.procedure.StatementsHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.unstring.UnstringStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class UnstringStatementHandlerImpl extends StatementHandlerImpl<UnstringStatement>
		implements UnstringStatementHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolPictureLengthService pictureLengthService;

	@Inject
	private StatementsHandler statementsHandler;

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

	protected char determineDelimitedByChar(final Sending sending, final UnstringStatement statement,
			final CobolInterpreterParams params) {
		final DelimitedByPhrase delimitedByPhrase = sending.getDelimitedByPhrase();
		final CobolValue delimitedByValue = valueStmtService.getValue(delimitedByPhrase.getDelimitedByValueStmt(),
				params.getState().getStorage());
		final String delimitedByString = valueService.getAsString(delimitedByValue, statement.getProgramUnit());
		return delimitedByString.charAt(0);
	}

	protected String determineSendingString(final Sending sending, final UnstringStatement statement,
			final CobolInterpreterParams params) {
		final DataDescriptionEntry sendingDataDescriptionEntry = dataDescriptionEntryService
				.getDataDescriptionEntry(sending.getSendingCall());
		final CobolValue sendingValue = storageService.getValue(sendingDataDescriptionEntry,
				params.getState().getStorage());
		return valueService.getAsString(sendingValue, statement.getProgramUnit());
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.UNSTRING;
	}

	private boolean isOverflow(final String intoString, final DataDescriptionEntry dataDescriptionEntry) {
		final Integer length = pictureLengthService.getLength(dataDescriptionEntry);
		return length != null && intoString.length() > length;
	}

	@Override
	public void run(final UnstringStatement statement, final CobolInterpreterParams params) {
		final Sending sending = statement.getSending();

		final String sendingString = determineSendingString(sending, statement, params);
		final char delimitedByChar = determineDelimitedByChar(sending, statement, params);

		final String[] parts = sendingString.split(Pattern.quote(Character.toString(delimitedByChar)));
		final IntoPhrase intoPhrase = statement.getIntoPhrase();
		final List<Into> intos = intoPhrase.getIntos();

		final OnOverflowPhrase onOverflowPhrase = statement.getOnOverflowPhrase();
		final NotOnOverflowPhrase notOnOverflowPhrase = statement.getNotOnOverflowPhrase();
		boolean isOverflow = false;

		for (int i = 0; i < parts.length; i++) {
			final String part = parts[i];

			final Into into = intos.get(i);
			final DataDescriptionEntry intoDataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(into.getIntoCall());

			isOverflow = isOverflow(part, intoDataDescriptionEntry);

			if (isOverflow) {
				break;
			}

			storageService.putValue(intoDataDescriptionEntry, CobolStringValueImpl.of(part),
					params.getState().getStorage());
		}

		if (isOverflow && onOverflowPhrase != null) {
			statementsHandler.run(onOverflowPhrase.getStatements(), params);
		} else if (notOnOverflowPhrase != null) {
			statementsHandler.run(notOnOverflowPhrase.getStatements(), params);
		}
	}
}
