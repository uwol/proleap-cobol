package io.proleap.cobol.interpreter.handlers.procedure.subtract.impl;

import java.math.BigDecimal;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.subtract.Giving;
import io.proleap.cobol.asg.metamodel.procedure.subtract.Minuend;
import io.proleap.cobol.asg.metamodel.procedure.subtract.MinuendCorresponding;
import io.proleap.cobol.asg.metamodel.procedure.subtract.MinuendGiving;
import io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractCorrespondingStatement;
import io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractFromGivingStatement;
import io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractFromStatement;
import io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractStatement;
import io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractStatement.SubtractType;
import io.proleap.cobol.asg.metamodel.procedure.subtract.Subtrahend;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.impl.CobolDecimalValueImpl;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.subtract.SubtractStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class SubtractStatementHandlerImpl extends StatementHandlerImpl<SubtractStatement>
		implements SubtractStatementHandler {

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
		return StatementTypeEnum.SUBTRACT;
	}

	protected void run(final SubtractCorrespondingStatement subtractCorrespondingStatement,
			final CobolInterpreterParams params) {
		final MinuendCorresponding minuendCorresponding = subtractCorrespondingStatement.getMinuend();

		final DataDescriptionEntryGroup minuend = (DataDescriptionEntryGroup) dataDescriptionEntryService
				.getDataDescriptionEntry(minuendCorresponding.getMinuendCall());
		final DataDescriptionEntryGroup subtrahend = (DataDescriptionEntryGroup) dataDescriptionEntryService
				.getDataDescriptionEntry(subtractCorrespondingStatement.getSubtrahendCall());

		for (final DataDescriptionEntry subtrahendChild : subtrahend.getDataDescriptionEntries()) {
			final String name = subtrahendChild.getName();
			final DataDescriptionEntry minuendChild = minuend.getDataDescriptionEntry(name);

			if (minuendChild != null) {
				final CobolValue minuendValue = storageService.getValue(minuendChild, params.getState().getStorage());
				final BigDecimal minuendDecimal = valueService.getAsDecimal(minuendValue);

				final CobolValue subtrahendValue = storageService.getValue(subtrahendChild,
						params.getState().getStorage());
				final BigDecimal subtrahendDecimal = valueService.getAsDecimal(subtrahendValue);

				final BigDecimal result = minuendDecimal.subtract(subtrahendDecimal);
				storageService.putValue(minuendChild, CobolDecimalValueImpl.of(result), params.getState().getStorage());
			}
		}
	}

	protected void run(final SubtractFromGivingStatement subtractFromGivingStatement,
			final CobolInterpreterParams params) {
		final MinuendGiving minuendGiving = subtractFromGivingStatement.getMinuend();
		final CobolValue minuendValue = valueStmtService.getValue(minuendGiving.getMinuendValueStmt(),
				params.getState().getStorage());
		final BigDecimal minuendDecimal = valueService.getAsDecimal(minuendValue);

		BigDecimal result = minuendDecimal;

		for (final Subtrahend subtrahend : subtractFromGivingStatement.getSubtrahends()) {
			final ValueStmt subtrahendValueStmt = subtrahend.getSubtrahendValueStmt();
			final CobolValue subtrahendValue = valueStmtService.getValue(subtrahendValueStmt,
					params.getState().getStorage());
			final BigDecimal subtrahendDecimal = valueService.getAsDecimal(subtrahendValue);

			result = result.subtract(subtrahendDecimal);
		}

		for (final Giving giving : subtractFromGivingStatement.getGivings()) {
			final DataDescriptionEntry resultDataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(giving.getGivingCall());
			storageService.putValue(resultDataDescriptionEntry, CobolDecimalValueImpl.of(result),
					params.getState().getStorage());
		}
	}

	protected void run(final SubtractFromStatement subtractFromStatement, final CobolInterpreterParams params) {
		for (final Minuend minuend : subtractFromStatement.getMinuends()) {
			final Call minuendCall = minuend.getMinuendCall();

			final DataDescriptionEntry minuendDataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(minuendCall);

			for (final Subtrahend subtrahend : subtractFromStatement.getSubtrahends()) {
				final ValueStmt subtrahendValueStmt = subtrahend.getSubtrahendValueStmt();
				final CobolValue subtrahendValue = valueStmtService.getValue(subtrahendValueStmt,
						params.getState().getStorage());
				final BigDecimal subtrahendDecimal = valueService.getAsDecimal(subtrahendValue);

				final CobolValue minuendValue = storageService.getValue(minuendDataDescriptionEntry,
						params.getState().getStorage());
				final BigDecimal minuendDecimal = valueService.getAsDecimal(minuendValue);

				final BigDecimal result = minuendDecimal.subtract(subtrahendDecimal);
				storageService.putValue(minuendDataDescriptionEntry, CobolDecimalValueImpl.of(result),
						params.getState().getStorage());
			}
		}
	}

	@Override
	public void run(final SubtractStatement statement, final CobolInterpreterParams params) {
		final SubtractType subtractType = statement.getSubtractType();

		switch (subtractType) {
		case CORRESPONDING:
			final SubtractCorrespondingStatement subtractCorrespondingStatement = statement
					.getSubtractCorrespondingStatement();
			run(subtractCorrespondingStatement, params);
			break;
		case FROM:
			final SubtractFromStatement subtractFromStatement = statement.getSubtractFromStatement();
			run(subtractFromStatement, params);
			break;
		case FROM_GIVING:
			final SubtractFromGivingStatement subtractFromGivingStatement = statement.getSubtractFromGivingStatement();
			run(subtractFromGivingStatement, params);
			break;
		}
	}
}
