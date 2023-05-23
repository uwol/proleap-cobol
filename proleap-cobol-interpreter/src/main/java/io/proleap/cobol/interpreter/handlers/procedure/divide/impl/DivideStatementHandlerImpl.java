package io.proleap.cobol.interpreter.handlers.procedure.divide.impl;

import java.math.BigDecimal;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.divide.DivideByGivingStatement;
import io.proleap.cobol.asg.metamodel.procedure.divide.DivideIntoGivingStatement;
import io.proleap.cobol.asg.metamodel.procedure.divide.DivideIntoStatement;
import io.proleap.cobol.asg.metamodel.procedure.divide.DivideStatement;
import io.proleap.cobol.asg.metamodel.procedure.divide.DivideStatement.DivideType;
import io.proleap.cobol.asg.metamodel.procedure.divide.Giving;
import io.proleap.cobol.asg.metamodel.procedure.divide.GivingPhrase;
import io.proleap.cobol.asg.metamodel.procedure.divide.Into;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.impl.CobolDecimalValueImpl;
import io.proleap.cobol.interpreter.handlers.procedure.divide.DivideStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class DivideStatementHandlerImpl extends StatementHandlerImpl<DivideStatement>
		implements DivideStatementHandler {

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

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.DIVIDE;
	}

	protected void run(final DivideByGivingStatement divideByGivingStatement, final DivideStatement statement,
			final CobolInterpreterParams params) {
		final CobolValue operandValue = valueStmtService.getValue(statement.getOperandValueStmt(),
				params.getState().getStorage());
		final BigDecimal operandDecimal = valueService.getAsDecimal(operandValue);

		final CobolValue byValue = valueStmtService.getValue(divideByGivingStatement.getByValueStmt(),
				params.getState().getStorage());
		final BigDecimal byDecimal = valueService.getAsDecimal(byValue);

		final BigDecimal result = operandDecimal.divide(byDecimal);

		final GivingPhrase givingPhrase = divideByGivingStatement.getGivingPhrase();

		if (givingPhrase == null) {
			final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(statement.getOperandValueStmt());
			storageService.putValue(dataDescriptionEntry, CobolDecimalValueImpl.of(result),
					params.getState().getStorage());
		} else {
			for (final Giving giving : givingPhrase.getGivings()) {
				final DataDescriptionEntry givingDataDescriptionEntry = dataDescriptionEntryService
						.getDataDescriptionEntry(giving.getGivingCall());
				storageService.putValue(givingDataDescriptionEntry, CobolDecimalValueImpl.of(result),
						params.getState().getStorage());
			}
		}
	}

	protected void run(final DivideIntoGivingStatement divideIntoGivingStatement, final DivideStatement statement,
			final CobolInterpreterParams params) {
		final GivingPhrase givingPhrase = divideIntoGivingStatement.getGivingPhrase();

		final CobolValue operandValue = valueStmtService.getValue(statement.getOperandValueStmt(),
				params.getState().getStorage());
		final BigDecimal operandDecimal = valueService.getAsDecimal(operandValue);

		final CobolValue intoValue = valueStmtService.getValue(divideIntoGivingStatement.getIntoValueStmt(),
				params.getState().getStorage());
		final BigDecimal intoDecimal = valueService.getAsDecimal(intoValue);

		final BigDecimal result = intoDecimal.divide(operandDecimal);

		for (final Giving giving : givingPhrase.getGivings()) {
			final DataDescriptionEntry givingDataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(giving.getGivingCall());
			storageService.putValue(givingDataDescriptionEntry, CobolDecimalValueImpl.of(result),
					params.getState().getStorage());
		}
	}

	protected void run(final DivideIntoStatement divideIntoStatement, final DivideStatement statement,
			final CobolInterpreterParams params) {
		final CobolValue operandValue = valueStmtService.getValue(statement.getOperandValueStmt(),
				params.getState().getStorage());
		final BigDecimal operandDecimal = valueService.getAsDecimal(operandValue);

		for (final Into into : divideIntoStatement.getIntos()) {
			final DataDescriptionEntry intoOperand = dataDescriptionEntryService
					.getDataDescriptionEntry(into.getGivingCall());
			final CobolValue intoValue = storageService.getValue(intoOperand, params.getState().getStorage());
			final BigDecimal intoDecimal = valueService.getAsDecimal(intoValue);

			final BigDecimal result = intoDecimal.divide(operandDecimal);
			storageService.putValue(intoOperand, CobolDecimalValueImpl.of(result), params.getState().getStorage());
		}
	}

	@Override
	public void run(final DivideStatement statement, final CobolInterpreterParams params) {
		final DivideType divideType = statement.getDivideType();

		switch (divideType) {
		case BY_GIVING:
			run(statement.getDivideByGivingStatement(), statement, params);
			break;
		case INTO:
			run(statement.getDivideIntoStatement(), statement, params);
			break;
		case INTO_GIVING:
			run(statement.getDivideIntoGivingStatement(), statement, params);
			break;
		}
	}
}
