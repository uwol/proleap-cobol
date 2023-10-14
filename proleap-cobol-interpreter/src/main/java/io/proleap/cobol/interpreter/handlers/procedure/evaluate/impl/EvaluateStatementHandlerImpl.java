package io.proleap.cobol.interpreter.handlers.procedure.evaluate.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.AlsoSelect;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.Condition;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.Condition.ConditionType;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.EvaluateStatement;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.Select;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.Through;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.Value;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.When;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.WhenOther;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.WhenPhrase;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.handlers.procedure.StatementsHandler;
import io.proleap.cobol.interpreter.handlers.procedure.evaluate.EvaluateStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class EvaluateStatementHandlerImpl extends StatementHandlerImpl<EvaluateStatement>
		implements EvaluateStatementHandler {

	@Inject
	private StatementsHandler statementsHandler;

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
		return StatementTypeEnum.EVALUATE;
	}

	@Override
	public void run(final EvaluateStatement statement, final CobolInterpreterParams params) {
		final List<CobolValue> selectionObjects = new ArrayList<>();

		final Select select = statement.getSelect();
		final CobolValue selectValue = valueStmtService.getValue(select.getSelectValueStmt(),
				params.getState().getStorage());

		if (selectValue != null) {
			selectionObjects.add(selectValue);
		}

		for (final AlsoSelect alsoSelect : statement.getAlsoSelects()) {
			final Select alsoSelectSelect = alsoSelect.getSelect();
			final CobolValue alsoSelectSelectValue = valueStmtService.getValue(alsoSelectSelect.getSelectValueStmt(),
					params.getState().getStorage());

			if (alsoSelectSelectValue != null) {
				selectionObjects.add(alsoSelectSelectValue);
			}
		}

		if (!selectionObjects.isEmpty()) {
			for (final WhenPhrase whenPhrase : statement.getWhenPhrases()) {
				boolean matching = false;

				for (final When when : whenPhrase.getWhens()) {
					final Condition whenCondition = when.getCondition();
					final ConditionType conditionType = whenCondition.getConditionType();

					switch (conditionType) {
					case ANY:
						matching = true;
						break;
					case BOOLEAN:
					case CONDITION:
						final CobolValue conditionValue = valueStmtService
								.getValue(whenCondition.getConditionValueStmt(), params.getState().getStorage());

						matching = selectionObjects.stream().map(object -> {
							return valueService.getValue(object);
						}).collect(Collectors.toList()).contains(valueService.getValue(conditionValue));
						break;
					case VALUE:
					case VALUE_THROUGH:
						final Value whenConditionValue = whenCondition.getValue();
						final Through whenConditionThrough = whenCondition.getThrough();
						final CobolValue value = valueStmtService.getValue(whenConditionValue.getValueStmt(),
								params.getState().getStorage());
						final CobolValue selectionObject = selectionObjects.get(0);

						if (whenConditionThrough == null) {
							final Object value1 = valueService.getValue(value);
							final Object value2 = valueService.getValue(selectionObject);
							final boolean equalsValue = value1 != null && value1.equals(value2);

							matching = whenCondition.isNot() ? !equalsValue : equalsValue;
						} else {
							final Value throughValue = whenConditionThrough.getValue();
							final CobolValue throughValueValue = valueStmtService.getValue(throughValue.getValueStmt(),
									params.getState().getStorage());

							final BigDecimal decimalValue = valueService.getAsDecimal(value);
							final BigDecimal decimalThroughValue = valueService.getAsDecimal(throughValueValue);
							final BigDecimal decimalSelectionObject = valueService.getAsDecimal(selectionObject);

							final boolean greaterThanLowerBound = decimalValue.compareTo(decimalSelectionObject) <= 0;
							final boolean lowerThanUpperBound = decimalThroughValue
									.compareTo(decimalSelectionObject) >= 0;
							matching = greaterThanLowerBound && lowerThanUpperBound;
						}
						break;
					}
				}

				if (matching) {
					statementsHandler.run(whenPhrase.getStatements(), params);
					return;
				}
			}
		}

		final WhenOther whenOther = statement.getWhenOther();

		if (whenOther != null) {
			statementsHandler.run(whenOther.getStatements(), params);
			return;
		}
	}
}
