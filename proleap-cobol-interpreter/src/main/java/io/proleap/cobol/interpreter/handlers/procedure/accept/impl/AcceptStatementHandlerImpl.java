package io.proleap.cobol.interpreter.handlers.procedure.accept.impl;

import java.io.Console;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Queue;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.accept.AcceptFromDateStatement;
import io.proleap.cobol.asg.metamodel.procedure.accept.AcceptFromDateStatement.DateType;
import io.proleap.cobol.asg.metamodel.procedure.accept.AcceptStatement;
import io.proleap.cobol.asg.metamodel.procedure.accept.AcceptStatement.AcceptType;
import io.proleap.cobol.asg.util.StringUtils;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.impl.CobolStringValueImpl;
import io.proleap.cobol.interpreter.handlers.procedure.accept.AcceptStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class AcceptStatementHandlerImpl extends StatementHandlerImpl<AcceptStatement>
		implements AcceptStatementHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolStorageService storageService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	private String getDayOfYear(final ZonedDateTime zonedDateTime) {
		return StringUtils.leftPad(
				String.valueOf(Integer.parseInt(DateTimeFormatter.ofPattern("D").format(zonedDateTime)) - 1), 3, "0");
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.ACCEPT;
	}

	@Override
	public void run(final AcceptStatement statement, final CobolInterpreterParams params) {
		final DataDescriptionEntry toDataDescriptionEntry = dataDescriptionEntryService
				.getDataDescriptionEntry(statement.getAcceptCall());
		final AcceptType acceptType = statement.getAcceptType();

		switch (acceptType) {
		case DATE:
			final AcceptFromDateStatement acceptFromDateStatement = statement.getAcceptFromDateStatement();
			run(toDataDescriptionEntry, acceptFromDateStatement, params);
			break;
		case FROM_ESCAPE_KEY:
			break;
		case MESSAGE_COUNT:
			break;
		case MNEMONIC:
			break;
		case NO_FROM:
			runNoFrom(toDataDescriptionEntry, params);
			break;
		default:
			break;
		}

		params.getState().getConsole().setNoAdvancing(false);
	}

	protected void run(final DataDescriptionEntry toDataDescriptionEntry,
			final AcceptFromDateStatement acceptFromDateStatement, final CobolInterpreterParams params) {
		final DateType dateType = acceptFromDateStatement.getDateType();
		final ZonedDateTime zonedDateTime = ZonedDateTime.now(ZoneId.of("UTC"));
		final String valueString;

		switch (dateType) {
		case DATE:
			valueString = DateTimeFormatter.ofPattern("yyMMdd").format(zonedDateTime);
			break;
		case DATE_YYYYMMDD:
			valueString = DateTimeFormatter.ofPattern("yyyyMMdd").format(zonedDateTime);
			break;
		case DAY:
			valueString = DateTimeFormatter.ofPattern("yy").format(zonedDateTime) + getDayOfYear(zonedDateTime);
			break;
		case DAY_YYYYMMDD:
			valueString = DateTimeFormatter.ofPattern("yyyyMMdd").format(zonedDateTime);
			break;
		case MMDDYYYY:
			valueString = DateTimeFormatter.ofPattern("MMddyyyy").format(zonedDateTime);
			break;
		case TIME:
			valueString = DateTimeFormatter.ofPattern("hhmmssSS").format(zonedDateTime);
			break;
		case TIMER:
			valueString = null;
			break;
		case TODAYS_DATE:
			valueString = null;
			break;
		case TODAYS_DATE_MMDDYYYY:
			valueString = DateTimeFormatter.ofPattern("MMddyyyy").format(zonedDateTime);
			break;
		case TODAYS_NAME:
			valueString = null;
			break;
		case YEAR:
			valueString = DateTimeFormatter.ofPattern("yyyy").format(zonedDateTime);
			break;
		case YYYYDDD:
			valueString = DateTimeFormatter.ofPattern("yyyy").format(zonedDateTime) + getDayOfYear(zonedDateTime);
			break;
		case YYYYMMDD:
			valueString = DateTimeFormatter.ofPattern("yyyyMMdd").format(zonedDateTime);
			break;
		default:
			valueString = null;
			break;
		}

		final CobolValue value = CobolStringValueImpl.of(valueString);
		storageService.putValue(toDataDescriptionEntry, value, params.getState().getStorage());
	}

	protected void runNoFrom(final DataDescriptionEntry toDataDescriptionEntry, final CobolInterpreterParams params) {
		final Queue<String> acceptParams = params.getAcceptParams();
		final String input;

		if (acceptParams != null && !acceptParams.isEmpty()) {
			input = acceptParams.poll();
		} else {
			final Console console = System.console();

			if (console == null) {
				input = null;
			} else {
				input = console.readLine();
			}
		}

		if (input != null) {
			final CobolValue value = CobolStringValueImpl.of(input);
			storageService.putValue(toDataDescriptionEntry, value, params.getState().getStorage());
		}
	}
}
