package io.proleap.cobol.interpreter.handlers.procedure.multiply.impl;

import java.math.BigDecimal;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.multiply.ByOperand;
import io.proleap.cobol.asg.metamodel.procedure.multiply.ByPhrase;
import io.proleap.cobol.asg.metamodel.procedure.multiply.GivingOperand;
import io.proleap.cobol.asg.metamodel.procedure.multiply.GivingPhrase;
import io.proleap.cobol.asg.metamodel.procedure.multiply.GivingResult;
import io.proleap.cobol.asg.metamodel.procedure.multiply.MultiplyStatement;
import io.proleap.cobol.asg.metamodel.procedure.multiply.MultiplyStatement.MultiplyType;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.impl.CobolDecimalValueImpl;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.multiply.MultiplyStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class MultiplyStatementHandlerImpl extends StatementHandlerImpl<MultiplyStatement>
		implements MultiplyStatementHandler {

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
		return StatementTypeEnum.MULTIPLY;
	}

	protected void run(final ByPhrase byPhrase, final MultiplyStatement statement,
			final CobolInterpreterParams params) {
		final CobolValue operand = valueStmtService.getValue(statement.getOperandValueStmt(),
				params.getState().getStorage());
		final BigDecimal operandValue = valueService.getAsDecimal(operand);

		for (final ByOperand byOperand : byPhrase.getByOperands()) {
			final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(byOperand.getOperandCall());
			final CobolValue byValue = storageService.getValue(dataDescriptionEntry, params.getState().getStorage());
			final BigDecimal byDecimal = valueService.getAsDecimal(byValue);
			final BigDecimal result = byDecimal.multiply(operandValue);

			storageService.putValue(dataDescriptionEntry, CobolDecimalValueImpl.of(result),
					params.getState().getStorage());
		}
	}

	protected void run(final GivingPhrase givingPhrase, final MultiplyStatement statement,
			final CobolInterpreterParams params) {
		final CobolValue operand = valueStmtService.getValue(statement.getOperandValueStmt(),
				params.getState().getStorage());

		final BigDecimal operandDecimal = valueService.getAsDecimal(operand);
		final GivingOperand givingOperand = givingPhrase.getGivingOperand();

		final BigDecimal givingDecimal = valueService.getAsDecimal(
				valueStmtService.getValue(givingOperand.getOperandValueStmt(), params.getState().getStorage()));
		final BigDecimal result = operandDecimal.multiply(givingDecimal);

		for (final GivingResult givingResult : givingPhrase.getGivingResults()) {
			final DataDescriptionEntry givingDataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(givingResult.getResultCall());
			storageService.putValue(givingDataDescriptionEntry, CobolDecimalValueImpl.of(result),
					params.getState().getStorage());
		}
	}

	@Override
	public void run(final MultiplyStatement statement, final CobolInterpreterParams params) {
		final MultiplyType multiplyType = statement.getMultiplyType();

		switch (multiplyType) {
		case BY:
			final ByPhrase byPhrase = statement.getByPhrase();
			run(byPhrase, statement, params);
			break;
		case BY_GIVING:
			final GivingPhrase givingPhrase = statement.getGivingPhrase();
			run(givingPhrase, statement, params);
			break;
		}
	}
}
