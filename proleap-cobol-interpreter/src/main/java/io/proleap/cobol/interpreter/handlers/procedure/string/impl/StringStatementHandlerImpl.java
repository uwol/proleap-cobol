package io.proleap.cobol.interpreter.handlers.procedure.string.impl;

import java.math.BigDecimal;
import java.util.regex.Pattern;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.NotOnOverflowPhrase;
import io.proleap.cobol.asg.metamodel.procedure.OnOverflowPhrase;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.string.DelimitedByPhrase;
import io.proleap.cobol.asg.metamodel.procedure.string.DelimitedByPhrase.DelimitedByType;
import io.proleap.cobol.asg.metamodel.procedure.string.ForPhrase;
import io.proleap.cobol.asg.metamodel.procedure.string.IntoPhrase;
import io.proleap.cobol.asg.metamodel.procedure.string.Sendings;
import io.proleap.cobol.asg.metamodel.procedure.string.Sendings.SendingsType;
import io.proleap.cobol.asg.metamodel.procedure.string.StringStatement;
import io.proleap.cobol.asg.metamodel.procedure.string.WithPointerPhrase;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.datadescription.CobolPictureLengthService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.impl.CobolDecimalValueImpl;
import io.proleap.cobol.commons.value.domain.impl.CobolStringValueImpl;
import io.proleap.cobol.interpreter.handlers.procedure.StatementsHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.string.StringStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class StringStatementHandlerImpl extends StatementHandlerImpl<StringStatement>
		implements StringStatementHandler {

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

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.STRING;
	}

	private boolean isOverflow(final String intoString, final DataDescriptionEntry dataDescriptionEntry) {
		final Integer length = pictureLengthService.getLength(dataDescriptionEntry);
		return length != null && intoString.length() > length;
	}

	@Override
	public void run(final StringStatement statement, final CobolInterpreterParams params) {
		final StringBuffer intoSb = new StringBuffer();

		for (final Sendings sendings : statement.getSendings()) {
			final SendingsType sendingsType = sendings.getSendingsType();

			for (final ValueStmt sendingValueStmt : sendings.getSendingValueStmts()) {
				final CobolValue sendingValue = valueStmtService.getValue(sendingValueStmt,
						params.getState().getStorage());
				final String sendingString = valueService.getAsString(sendingValue, statement.getProgramUnit());
				final String stringToAppend;

				switch (sendingsType) {
				case FOR:
					final ForPhrase forPhrase = sendings.getForPhrase();
					final CobolValue forValue = valueStmtService.getValue(forPhrase.getForValueStmt(),
							params.getState().getStorage());
					final BigDecimal forDecimal = valueService.getAsDecimal(forValue);
					stringToAppend = sendingString.repeat(forDecimal.intValue());
					break;
				case DELIMITED_BY:
				default:
					final DelimitedByPhrase delimitedByPhrase = sendings.getDelimitedByPhrase();
					final DelimitedByType delimitedByType = delimitedByPhrase.getDelimitedByType();

					switch (delimitedByType) {
					case CHARACTERS:
						final CobolValue charactersValue = valueStmtService
								.getValue(delimitedByPhrase.getCharactersValueStmt(), params.getState().getStorage());
						final String charactersString = valueService.getString(charactersValue);
						final String[] splitted = sendingString.split(Pattern.quote(charactersString));
						stringToAppend = splitted.length > 0 ? splitted[0] : "";
						break;
					case SIZE:
					default:
						stringToAppend = sendingString;
						break;
					}
					break;
				}

				intoSb.append(stringToAppend);
			}
		}

		final String intoString = intoSb.toString();
		final IntoPhrase intoPhrase = statement.getIntoPhrase();
		final DataDescriptionEntry intoDataDescriptionEntry = dataDescriptionEntryService
				.getDataDescriptionEntry(intoPhrase.getIntoCall());

		if (isOverflow(intoString, intoDataDescriptionEntry)) {
			final OnOverflowPhrase onOverflowPhrase = statement.getOnOverflowPhrase();

			if (onOverflowPhrase != null) {
				statementsHandler.run(onOverflowPhrase.getStatements(), params);
			}
		} else {
			storageService.putValue(intoDataDescriptionEntry, CobolStringValueImpl.of(intoString),
					params.getState().getStorage());

			final WithPointerPhrase withPointerPhrase = statement.getWithPointerPhrase();

			if (withPointerPhrase != null) {
				final DataDescriptionEntry pointerDataDescriptionEntry = dataDescriptionEntryService
						.getDataDescriptionEntry(withPointerPhrase.getPointerCall());
				final int pointer = intoString.length() + 1;
				storageService.putValue(pointerDataDescriptionEntry,
						CobolDecimalValueImpl.of(BigDecimal.valueOf(pointer)), params.getState().getStorage());
			}

			final NotOnOverflowPhrase notOnOverflowPhrase = statement.getNotOnOverflowPhrase();

			if (notOnOverflowPhrase != null) {
				statementsHandler.run(notOnOverflowPhrase.getStatements(), params);
			}
		}
	}
}
