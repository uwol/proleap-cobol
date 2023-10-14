package io.proleap.cobol.interpreter.handlers.procedure.add.impl;

import java.math.BigDecimal;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.add.AddCorrespondingStatement;
import io.proleap.cobol.asg.metamodel.procedure.add.AddStatement;
import io.proleap.cobol.asg.metamodel.procedure.add.AddStatement.AddType;
import io.proleap.cobol.asg.metamodel.procedure.add.AddToGivingStatement;
import io.proleap.cobol.asg.metamodel.procedure.add.AddToStatement;
import io.proleap.cobol.asg.metamodel.procedure.add.From;
import io.proleap.cobol.asg.metamodel.procedure.add.To;
import io.proleap.cobol.asg.metamodel.procedure.add.ToGiving;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.impl.CobolDecimalValueImpl;
import io.proleap.cobol.interpreter.handlers.procedure.add.AddStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class AddStatementHandlerImpl extends StatementHandlerImpl<AddStatement> implements AddStatementHandler {

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
		return StatementTypeEnum.ADD;
	}

	protected void run(final AddCorrespondingStatement addCorrespondingStatement, final CobolInterpreterParams params) {
		final To to = addCorrespondingStatement.getTo();

		final DataDescriptionEntryGroup fromGroup = (DataDescriptionEntryGroup) dataDescriptionEntryService
				.getDataDescriptionEntry(addCorrespondingStatement.getFromCall());
		final DataDescriptionEntryGroup toGroup = (DataDescriptionEntryGroup) dataDescriptionEntryService
				.getDataDescriptionEntry(to.getToCall());

		for (final DataDescriptionEntry fromChild : fromGroup.getDataDescriptionEntries()) {
			final String name = fromChild.getName();
			final DataDescriptionEntry toChild = toGroup.getDataDescriptionEntry(name);

			if (toChild != null) {
				final CobolValue fromValue = storageService.getValue(fromChild, params.getState().getStorage());
				final BigDecimal fromDecimal = valueService.getAsDecimal(fromValue);

				final CobolValue toValue = storageService.getValue(toChild, params.getState().getStorage());
				final BigDecimal toDecimal = valueService.getAsDecimal(toValue);

				final BigDecimal result = toDecimal.add(fromDecimal);
				storageService.putValue(toChild, CobolDecimalValueImpl.of(result), params.getState().getStorage());
			}
		}
	}

	@Override
	public void run(final AddStatement statement, final CobolInterpreterParams params) {
		final AddType addType = statement.getAddType();

		switch (addType) {
		case CORRESPONDING:
			run(statement.getAddCorrespondingStatement(), params);
			break;
		case TO:
			run(statement.getAddToStatement(), params);
			break;
		case TO_GIVING:
			run(statement.getAddToGivingStatement(), params);
			break;
		}
	}

	protected void run(final AddToGivingStatement addToGivingStatement, final CobolInterpreterParams params) {
		BigDecimal addendLeft = BigDecimal.ZERO;

		for (final From from : addToGivingStatement.getFroms()) {
			final CobolValue fromValue = valueStmtService.getValue(from.getFromValueStmt(),
					params.getState().getStorage());
			final BigDecimal fromDecimal = valueService.getAsDecimal(fromValue);
			addendLeft = addendLeft.add(fromDecimal);
		}

		BigDecimal addendRight = BigDecimal.ZERO;

		for (final ToGiving to : addToGivingStatement.getTos()) {
			final CobolValue toValue = valueStmtService.getValue(to.getToValueStmt(), params.getState().getStorage());
			final BigDecimal toDecimal = valueService.getAsDecimal(toValue);
			addendRight = addendRight.add(toDecimal);
		}

		for (final io.proleap.cobol.asg.metamodel.procedure.add.Giving giving : addToGivingStatement.getGivings()) {
			final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(giving.getGivingCall());

			final BigDecimal result = addendLeft.add(addendRight);
			storageService.putValue(dataDescriptionEntry, CobolDecimalValueImpl.of(result),
					params.getState().getStorage());
		}
	}

	protected void run(final AddToStatement addToStatement, final CobolInterpreterParams params) {
		BigDecimal summand = BigDecimal.ZERO;

		for (final From from : addToStatement.getFroms()) {
			final CobolValue fromValue = valueStmtService.getValue(from.getFromValueStmt(),
					params.getState().getStorage());
			final BigDecimal fromDecimal = valueService.getAsDecimal(fromValue);

			if (fromDecimal != null) {
				summand = summand.add(fromDecimal);
			}
		}

		for (final To to : addToStatement.getTos()) {
			final DataDescriptionEntry toDataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(to.getToCall());
			final CobolValue toValue = storageService.getValue(toDataDescriptionEntry, params.getState().getStorage());
			final BigDecimal toDecimal = valueService.getAsDecimal(toValue);

			if (toDecimal != null) {
				final BigDecimal toResult = toDecimal.add(summand);

				storageService.putValue(toDataDescriptionEntry, CobolDecimalValueImpl.of(toResult),
						params.getState().getStorage());
			}
		}
	}
}
