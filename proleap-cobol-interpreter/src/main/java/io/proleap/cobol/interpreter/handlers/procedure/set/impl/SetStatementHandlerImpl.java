package io.proleap.cobol.interpreter.handlers.procedure.set.impl;

import java.math.BigDecimal;
import java.util.List;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.set.By;
import io.proleap.cobol.asg.metamodel.procedure.set.SetBy;
import io.proleap.cobol.asg.metamodel.procedure.set.SetBy.SetByType;
import io.proleap.cobol.asg.metamodel.procedure.set.SetStatement;
import io.proleap.cobol.asg.metamodel.procedure.set.SetStatement.SetType;
import io.proleap.cobol.asg.metamodel.procedure.set.SetTo;
import io.proleap.cobol.asg.metamodel.procedure.set.To;
import io.proleap.cobol.asg.metamodel.procedure.set.Value;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.CobolValue.CobolValueType;
import io.proleap.cobol.commons.value.domain.impl.CobolBooleanValueImpl;
import io.proleap.cobol.commons.value.domain.impl.CobolDecimalValueImpl;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.set.SetStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class SetStatementHandlerImpl extends StatementHandlerImpl<SetStatement> implements SetStatementHandler {

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
		return StatementTypeEnum.SET;
	}

	protected void run(final SetBy setBy, final CobolInterpreterParams params) {
		final SetByType setByType = setBy.getSetByType();
		final By by = setBy.getBy();

		for (final To to : setBy.getTos()) {
			final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(to.getToCall());

			final CobolValue origialValue = storageService.getValue(dataDescriptionEntry, params.getState().getStorage());
			final BigDecimal originalDecimalValue = valueService.getAsDecimal(origialValue);

			final BigDecimal byBigDecimalValue = valueService
					.getAsDecimal(valueStmtService.getValue(by.getByValueStmt(), params.getState().getStorage()));

			final BigDecimal resultDecimalValue;

			switch (setByType) {
			case DOWN:
				resultDecimalValue = originalDecimalValue.subtract(byBigDecimalValue);
				break;
			case UP:
			default:
				resultDecimalValue = originalDecimalValue.add(byBigDecimalValue);
				break;
			}

			storageService.putValue(dataDescriptionEntry, CobolDecimalValueImpl.of(resultDecimalValue),
					params.getState().getStorage());
		}
	}

	@Override
	public void run(final SetStatement statement, final CobolInterpreterParams params) {
		final SetType setType = statement.getSetType();

		switch (setType) {
		case BY:
			final SetBy setBy = statement.getSetBy();
			run(setBy, params);
			break;
		case TO:
			for (final SetTo setTo : statement.getSetTos()) {
				run(setTo, params);
			}
			break;
		default:
			break;
		}
	}

	protected void run(final SetTo setTo, final CobolInterpreterParams params) {
		final List<To> tos = setTo.getTos();
		final List<Value> values = setTo.getValues();

		for (int i = 0; i < tos.size(); i++) {
			final To to = tos.get(i);
			final Value value = values.get(i);

			final DataDescriptionEntry toDataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(to.getToCall());
			final CobolValue toValue = valueStmtService.getValue(value.getValueStmt(), params.getState().getStorage());

			if (DataDescriptionEntryType.CONDITION.equals(toDataDescriptionEntry.getDataDescriptionEntryType())
					&& CobolValueType.BOOLEAN.equals(toValue.getType()) && CobolBooleanValueImpl.TRUE.equals(toValue)) {
				setConditionTrue(toDataDescriptionEntry, params);
			} else {
				storageService.putValue(toDataDescriptionEntry, toValue, params.getState().getStorage());
			}
		}
	}

	protected void setConditionTrue(final DataDescriptionEntry toDataDescriptionEntry,
			final CobolInterpreterParams params) {
		final DataDescriptionEntryGroup parent = toDataDescriptionEntry.getParentDataDescriptionEntryGroup();
		final CobolValue conditionToValue = valueStmtService.getValueClauseValue(toDataDescriptionEntry,
				params.getState().getStorage());
		storageService.putValue(parent, conditionToValue, params.getState().getStorage());
	}
}
