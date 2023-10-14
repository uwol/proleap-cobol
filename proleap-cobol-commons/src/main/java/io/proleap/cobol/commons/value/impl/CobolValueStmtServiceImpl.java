package io.proleap.cobol.commons.value.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.FigurativeConstant;
import io.proleap.cobol.asg.metamodel.FigurativeConstant.FigurativeConstantType;
import io.proleap.cobol.asg.metamodel.IntegerLiteral;
import io.proleap.cobol.asg.metamodel.Literal;
import io.proleap.cobol.asg.metamodel.Literal.LiteralType;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryCondition;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.PictureClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.ValueClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.ValueInterval;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.BooleanLiteralValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.CallValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.ConditionValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.IntegerLiteralValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.LiteralValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Basis;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDiv;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDiv.MultDivType;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDivs;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.PlusMinus;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Power;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Powers;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Powers.PowersType;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.AndOrCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.AndOrCondition.AndOrConditionType;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.CombinableCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.ConditionNameReference;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.SimpleCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.ArithmeticComparison;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.CombinedComparison;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.CombinedCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.CombinedCondition.CombinedConditionType;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.RelationalOperator;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.RelationalOperator.RelationalOperatorType;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.SignCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.SignCondition.SignConditionType;
import io.proleap.cobol.asg.util.StringUtils;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.datadescription.CobolPictureLengthService;
import io.proleap.cobol.commons.datadescription.CobolValueProviderService;
import io.proleap.cobol.commons.type.CobolTypeEnum;
import io.proleap.cobol.commons.type.CobolTypeService;
import io.proleap.cobol.commons.value.CobolValueComparatorService;
import io.proleap.cobol.commons.value.CobolValueProvider;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.impl.CobolBooleanValueImpl;
import io.proleap.cobol.commons.value.domain.impl.CobolDecimalValueImpl;
import io.proleap.cobol.commons.value.domain.impl.CobolHighValueImpl;
import io.proleap.cobol.commons.value.domain.impl.CobolLowValueImpl;
import io.proleap.cobol.commons.value.domain.impl.CobolStringValueImpl;

@Singleton
public class CobolValueStmtServiceImpl implements CobolValueStmtService {

	@Inject
	private CobolPictureLengthService cobolPictureLengthService;

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private Optional<CobolValueProviderService> dataDescriptionEntryValueServiceOptional;

	@Inject
	private CobolTypeService typeService;

	@Inject
	private CobolValueComparatorService valueComparatorService;

	@Inject
	private CobolValueService valueService;

	public CobolValue getValue(final AndOrCondition andOrCondition, final CobolValueProvider valueProvider) {
		final CobolValue result;

		if (andOrCondition.getCombinableCondition() == null) {
			result = null;
		} else {
			final CobolValue combinableConditionValue = getValue(andOrCondition.getCombinableCondition(),
					valueProvider);
			final Boolean booleanCombinableConditionValue = valueService.getBoolean(combinableConditionValue);

			if (booleanCombinableConditionValue == null) {
				result = combinableConditionValue;
			} else if (andOrCondition.getAbbreviations().isEmpty()) {
				result = combinableConditionValue;
			} else {
				result = null;
			}
		}

		return result;
	}

	public CobolValue getValue(final ArithmeticComparison arithmeticComparison,
			final CobolValueProvider valueProvider) {
		final CobolValue valueLeft = getValue(arithmeticComparison.getArithmeticExpressionLeft(), valueProvider);
		final CobolValue valueRight = getValue(arithmeticComparison.getArithmeticExpressionRight(), valueProvider);
		final Boolean result;

		if (valueLeft == null || valueRight == null) {
			result = null;
		} else {
			final ProgramUnit programUnit = arithmeticComparison.getProgramUnit();
			final RelationalOperator operator = arithmeticComparison.getOperator();
			final RelationalOperatorType relationalOperatorType = operator.getRelationalOperatorType();

			switch (relationalOperatorType) {
			case NOT_EQUAL:
				result = valueComparatorService.notEqual(valueLeft, valueRight, programUnit);
				break;
			case EQUAL:
				result = valueComparatorService.equal(valueLeft, valueRight, programUnit);
				break;
			case GREATER:
				result = valueComparatorService.greater(valueLeft, valueRight, programUnit);
				break;
			case GREATER_OR_EQUAL:
				result = valueComparatorService.greaterOrEqual(valueLeft, valueRight, programUnit);
				break;
			case LESS:
				result = valueComparatorService.less(valueLeft, valueRight, programUnit);
				break;
			case LESS_OR_EQUAL:
				result = valueComparatorService.lessOrEqual(valueLeft, valueRight, programUnit);
				break;
			default:
				result = null;
				break;
			}
		}

		return CobolBooleanValueImpl.of(result);
	}

	public CobolValue getValue(final ArithmeticValueStmt arithmeticValueStmt, final CobolValueProvider valueProvider) {
		final CobolValue result;

		if (arithmeticValueStmt.getPlusMinus().isEmpty()) {
			result = getValue(arithmeticValueStmt.getMultDivs(), valueProvider);
		} else {
			BigDecimal numberValue = valueService
					.getDecimal(getValue(arithmeticValueStmt.getMultDivs(), valueProvider));

			if (numberValue == null) {
				result = null;
			} else {
				for (final PlusMinus plusMinusEntry : arithmeticValueStmt.getPlusMinus()) {
					final BigDecimal plusMinusEntryValue = valueService
							.getDecimal(getValue(plusMinusEntry, valueProvider));

					if (plusMinusEntryValue == null) {
						numberValue = null;
						break;
					} else {
						numberValue = numberValue.add(plusMinusEntryValue);
					}
				}

				result = CobolDecimalValueImpl.of(numberValue);
			}
		}

		return result;
	}

	public CobolValue getValue(final Basis basis, final CobolValueProvider valueProvider) {
		return basis.getBasisValueStmt() == null ? null : getValue(basis.getBasisValueStmt(), valueProvider);
	}

	public CobolValue getValue(final BooleanLiteralValueStmt booleanLiteralValueStmt,
			final CobolValueProvider valueProvider) {
		return booleanLiteralValueStmt.getLiteral() != null
				? CobolBooleanValueImpl.of(booleanLiteralValueStmt.getLiteral().getValue())
				: null;
	}

	public CobolValue getValue(final CallValueStmt callValueStmt, final CobolValueProvider valueProvider) {
		final CobolValue result;

		if (valueProvider == null) {
			result = null;
		} else {
			final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(callValueStmt);

			if (dataDescriptionEntry == null) {
				result = null;
			} else if (!dataDescriptionEntryValueServiceOptional.isPresent()) {
				result = null;
			} else {
				final CobolValueProviderService valueProviderService = dataDescriptionEntryValueServiceOptional.get();
				result = valueProviderService.getValue(dataDescriptionEntry, valueProvider);
			}
		}

		return result;
	}

	public CobolValue getValue(final CombinableCondition combinableCondition, final CobolValueProvider valueProvider) {
		final CobolValue simpleConditionValue = getValue(combinableCondition.getSimpleCondition(), valueProvider);
		final Boolean booleanSimpleConditionValue = valueService.getBoolean(simpleConditionValue);
		final CobolValue result;

		if (booleanSimpleConditionValue != null && combinableCondition.isNot()) {
			result = CobolBooleanValueImpl.of(!booleanSimpleConditionValue);
		} else if (booleanSimpleConditionValue != null) {
			result = CobolBooleanValueImpl.of(booleanSimpleConditionValue);
		} else {
			result = simpleConditionValue;
		}

		return result;
	}

	public CobolValue getValue(final CombinedComparison combinedComparison, final CobolValueProvider valueProvider) {
		final CobolValue expressionValue = getValue(combinedComparison.getArithmeticExpression(), valueProvider);
		final BigDecimal decimalExpressionValue = valueService.getDecimal(expressionValue);

		final List<BigDecimal> combinedConditionValues = getValue(combinedComparison.getCombinedCondition(),
				valueProvider);

		final Boolean result;

		if (decimalExpressionValue == null) {
			result = null;
		} else if (combinedConditionValues == null) {
			result = null;
		} else if (combinedConditionValues.isEmpty()) {
			result = null;
		} else {
			final RelationalOperator operator = combinedComparison.getOperator();
			final RelationalOperatorType relationalOperatorType = operator.getRelationalOperatorType();

			final CombinedCondition combinedCondition = combinedComparison.getCombinedCondition();
			final CombinedConditionType combinedConditionType = combinedCondition.getCombinedConditionType();
			Boolean value = null;

			if (CombinedConditionType.AND.equals(combinedConditionType)) {
				value = true;
			} else if (CombinedConditionType.OR.equals(combinedConditionType)) {
				value = false;
			} else {
				value = null;
			}

			if (value == null) {
			} else {
				for (final BigDecimal conditionValue : combinedConditionValues) {
					final Boolean conditionResult;

					if (RelationalOperatorType.EQUAL.equals(relationalOperatorType)) {
						conditionResult = decimalExpressionValue.equals(conditionValue);
					} else if (RelationalOperatorType.GREATER.equals(relationalOperatorType)) {
						conditionResult = decimalExpressionValue.compareTo(conditionValue) > 0;
					} else if (RelationalOperatorType.GREATER_OR_EQUAL.equals(relationalOperatorType)) {
						conditionResult = decimalExpressionValue.compareTo(conditionValue) >= 0;
					} else if (RelationalOperatorType.LESS.equals(relationalOperatorType)) {
						conditionResult = decimalExpressionValue.compareTo(conditionValue) < 0;
					} else if (RelationalOperatorType.LESS_OR_EQUAL.equals(relationalOperatorType)) {
						conditionResult = decimalExpressionValue.compareTo(conditionValue) <= 0;
					} else if (RelationalOperatorType.NOT_EQUAL.equals(relationalOperatorType)) {
						conditionResult = decimalExpressionValue.compareTo(conditionValue) != 0;
					} else {
						conditionResult = null;
					}

					if (conditionResult == null) {
						value = null;
						break;
					} else if (CombinedConditionType.AND.equals(combinedConditionType)) {
						value = value && conditionResult;
					} else if (CombinedConditionType.OR.equals(combinedConditionType)) {
						value = value || conditionResult;
					}
				}
			}

			result = value;
		}

		return CobolBooleanValueImpl.of(result);
	}

	public List<BigDecimal> getValue(final CombinedCondition combinedCondition,
			final CobolValueProvider valueProvider) {
		final List<BigDecimal> result = new ArrayList<BigDecimal>();

		for (final ArithmeticValueStmt arithmeticValueStmt : combinedCondition.getArithmeticExpressions()) {
			final CobolValue arithmeticValue = getValue(arithmeticValueStmt, valueProvider);
			final BigDecimal decimalArithmeticValue = valueService.getDecimal(arithmeticValue);

			if (decimalArithmeticValue == null) {
				return null;
			} else {
				result.add(decimalArithmeticValue);
			}
		}

		return result;
	}

	public CobolValue getValue(final ConditionNameReference conditionNameReference,
			final CobolValueProvider valueProvider) {
		final Call conditionCall = conditionNameReference.getConditionCall();
		final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
				.getDataDescriptionEntry(conditionCall);
		final CobolValue result;

		if (dataDescriptionEntry == null) {
			result = null;
		} else if (!dataDescriptionEntryValueServiceOptional.isPresent()) {
			result = null;
		} else {
			final DataDescriptionEntryGroup parentDataDescriptionEntryGroup = dataDescriptionEntry
					.getParentDataDescriptionEntryGroup();

			if (parentDataDescriptionEntryGroup == null) {
				result = null;
			} else {
				final CobolValueProviderService valueProviderService = dataDescriptionEntryValueServiceOptional.get();
				final CobolValue parentValue = valueProviderService.getValue(parentDataDescriptionEntryGroup,
						valueProvider);
				final CobolValue conditionValue = getValueClauseValue(dataDescriptionEntry, valueProvider);
				final boolean matches = parentValue.equals(conditionValue);
				result = CobolBooleanValueImpl.of(matches);
			}
		}

		return result;
	}

	public CobolValue getValue(final ConditionValueStmt conditionValueStmt, final CobolValueProvider valueProvider) {
		final CobolValue combinableConditionValue = getValue(conditionValueStmt.getCombinableCondition(),
				valueProvider);
		final Boolean booleanCombinableConditionValue = valueService.getBoolean(combinableConditionValue);
		final CobolValue result;

		if (booleanCombinableConditionValue == null) {
			result = combinableConditionValue;
		} else if (conditionValueStmt.getAndOrConditions().isEmpty()) {
			result = combinableConditionValue;
		} else {
			Boolean value = booleanCombinableConditionValue;

			for (final AndOrCondition andOrCondition : conditionValueStmt.getAndOrConditions()) {
				final AndOrConditionType andOrConditionType = andOrCondition.getAndOrConditionType();
				final Boolean booleanAndOrConditionValue = valueService
						.getBoolean(getValue(andOrCondition, valueProvider));

				if (booleanAndOrConditionValue == null) {
					value = null;
					break;
				} else if (AndOrConditionType.AND.equals(andOrConditionType)) {
					value = value && booleanAndOrConditionValue;
				} else if (AndOrConditionType.OR.equals(andOrConditionType)) {
					value = value || booleanAndOrConditionValue;
				}
			}

			result = CobolBooleanValueImpl.of(value);
		}

		return result;
	}

	public CobolValue getValue(final FigurativeConstant figurativeConstant, final CobolValueProvider valueProvider) {
		final FigurativeConstantType figurativeConstantType = figurativeConstant.getFigurativeConstantType();
		final CobolValue result;

		switch (figurativeConstantType) {
		case ALL:
			result = null;
			break;
		case HIGH_VALUE:
		case HIGH_VALUES:
			result = new CobolHighValueImpl();
			break;
		case LOW_VALUE:
		case LOW_VALUES:
			result = new CobolLowValueImpl();
			break;
		case NULL:
		case NULLS:
			result = null;
			break;
		case QUOTE:
		case QUOTES:
			result = null;
			break;
		case SPACE:
		case SPACES:
			result = CobolStringValueImpl.of(WS);
			break;
		case ZERO:
		case ZEROES:
		case ZEROS:
			result = CobolDecimalValueImpl.of(BigDecimal.ZERO);
			break;
		default:
			result = null;
			break;
		}

		return result;
	}

	public CobolValue getValue(final IntegerLiteralValueStmt integerLiteralValueStmt,
			final CobolValueProvider valueProvider) {
		final IntegerLiteral literal = integerLiteralValueStmt.getLiteral();
		return literal == null ? null : CobolDecimalValueImpl.of(literal.getValue());
	}

	public CobolValue getValue(final Literal literal, final CobolValueProvider valueProvider) {
		final LiteralType literalType = literal.getLiteralType();
		final CobolValue result;

		switch (literalType) {
		case NON_NUMERIC:
			result = CobolStringValueImpl.of(literal.getNonNumericLiteral());
			break;
		case NUMERIC:
			result = CobolDecimalValueImpl.of(literal.getNumericLiteral().getValue());
			break;
		case BOOLEAN:
			result = CobolBooleanValueImpl.of(literal.getBooleanLiteral().getValue());
			break;
		case FIGURATIVE_CONSTANT:
			result = getValue(literal.getFigurativeConstant(), valueProvider);
			break;
		default:
			result = null;
			break;
		}

		return result;
	}

	public CobolValue getValue(final LiteralValueStmt literalValueStmt, final CobolValueProvider valueProvider) {
		final Literal literal = literalValueStmt.getLiteral();
		return literal == null ? null : getValue(literal, valueProvider);
	}

	public CobolValue getValue(final MultDiv multDiv, final CobolValueProvider valueProvider) {
		return multDiv.getPowers() == null ? null : getValue(multDiv.getPowers(), valueProvider);
	}

	public CobolValue getValue(final MultDivs multDivs, final CobolValueProvider valueProvider) {
		final CobolValue result;

		if (multDivs.getMultDivs().isEmpty()) {
			result = getValue(multDivs.getPowers(), valueProvider);
		} else {
			BigDecimal decimalValue = valueService.getDecimal(getValue(multDivs.getPowers(), valueProvider));

			if (decimalValue == null) {
				result = null;
			} else {
				for (final MultDiv multDiv : multDivs.getMultDivs()) {
					final BigDecimal multDivValue = valueService.getDecimal(getValue(multDiv, valueProvider));

					if (multDivValue == null) {
						decimalValue = null;
						break;
					} else if (MultDivType.MULT.equals(multDiv.getMultDivType())) {
						decimalValue = decimalValue.multiply(multDivValue);
					} else if (MultDivType.DIV.equals(multDiv.getMultDivType()) && multDivValue.signum() != 0) {
						decimalValue = decimalValue.divide(multDivValue);
					}
				}

				if (decimalValue == null) {
					result = null;
				} else {
					result = CobolDecimalValueImpl.of(decimalValue);
				}
			}
		}

		return result;
	}

	public CobolValue getValue(final PlusMinus plusMinus, final CobolValueProvider valueProvider) {
		final CobolValue result;

		switch (plusMinus.getPlusMinusType()) {
		case MINUS:
			final BigDecimal decimal = valueService.getDecimal(getValue(plusMinus.getMultDivs(), valueProvider));
			result = decimal == null ? null : CobolDecimalValueImpl.of(decimal.multiply(new BigDecimal(-1)));
			break;
		default:
			result = getValue(plusMinus.getMultDivs(), valueProvider);
			break;
		}

		return result;
	}

	public CobolValue getValue(final Power power, final CobolValueProvider valueProvider) {
		return power.getBasis() == null ? null : getValue(power.getBasis(), valueProvider);
	}

	public CobolValue getValue(final Powers powers, final CobolValueProvider valueProvider) {
		final BigDecimal basisDecimal = valueService.getDecimal(getValue(powers.getBasis(), valueProvider));
		final CobolValue result;

		if (basisDecimal == null) {
			result = getValue(powers.getBasis(), valueProvider);
		} else {
			final BigDecimal basisDecimalSigned;

			if (PowersType.MINUS.equals(powers.getPowersType())) {
				basisDecimalSigned = basisDecimal.multiply(new BigDecimal(-1));
			} else {
				basisDecimalSigned = basisDecimal;
			}

			if (powers.getPowers().isEmpty()) {
				result = CobolDecimalValueImpl.of(basisDecimalSigned);
			} else {
				BigDecimal value = basisDecimalSigned;

				for (final Power power : powers.getPowers()) {
					final BigDecimal powerValue = valueService.getDecimal(getValue(power, valueProvider));

					if (powerValue == null) {
						value = null;
						break;
					} else {
						final double doublePower = Math.pow(value.doubleValue(), powerValue.doubleValue());
						value = new BigDecimal(doublePower);
					}
				}

				result = CobolDecimalValueImpl.of(value);
			}
		}

		return result;
	}

	public CobolValue getValue(final RelationConditionValueStmt relationConditionValueStmt,
			final CobolValueProvider valueProvider) {
		final CobolValue result;

		if (relationConditionValueStmt.getSignCondition() != null) {
			result = getValue(relationConditionValueStmt.getSignCondition(), valueProvider);
		} else if (relationConditionValueStmt.getArithmeticComparison() != null) {
			result = getValue(relationConditionValueStmt.getArithmeticComparison(), valueProvider);
		} else if (relationConditionValueStmt.getCombinedComparison() != null) {
			result = getValue(relationConditionValueStmt.getCombinedComparison(), valueProvider);
		} else {
			result = null;
		}

		return result;
	}

	public CobolValue getValue(final SignCondition signCondition, final CobolValueProvider valueProvider) {
		final BigDecimal value = valueService
				.getDecimal(getValue(signCondition.getArithmeticExpression(), valueProvider));
		final SignConditionType signConditionType = signCondition.getSignConditionType();
		final boolean not = signCondition.getNot();

		final Boolean result;

		if (value == null) {
			result = null;
		} else if (SignConditionType.POSITIVE.equals(signConditionType) && not) {
			result = value.signum() <= 0;
		} else if (SignConditionType.NEGATIVE.equals(signConditionType) && not) {
			result = value.signum() >= 0;
		} else if (SignConditionType.ZERO.equals(signConditionType) && not) {
			result = value.signum() != 0;
		} else if (SignConditionType.POSITIVE.equals(signConditionType)) {
			result = value.signum() > 0;
		} else if (SignConditionType.NEGATIVE.equals(signConditionType)) {
			result = value.signum() < 0;
		} else if (SignConditionType.ZERO.equals(signConditionType)) {
			result = value.signum() == 0.0;
		} else {
			result = null;
		}

		return CobolBooleanValueImpl.of(result);
	}

	public CobolValue getValue(final SimpleCondition simpleCondition, final CobolValueProvider valueProvider) {
		final CobolValue result;

		if (simpleCondition.getCondition() != null) {
			result = getValue(simpleCondition.getCondition(), valueProvider);
		} else if (simpleCondition.getRelationCondition() != null) {
			result = getValue(simpleCondition.getRelationCondition(), valueProvider);
		} else if (simpleCondition.getClassCondition() != null) {
			result = getValue(simpleCondition.getClassCondition(), valueProvider);
		} else if (simpleCondition.getConditionNameReference() != null) {
			result = getValue(simpleCondition.getConditionNameReference(), valueProvider);
		} else {
			result = null;
		}

		return result;
	}

	private CobolValue getValue(final ValueClause valueClause, final CobolValueProvider valueProvider) {
		final List<ValueInterval> valueIntervals = valueClause.getValueIntervals();
		final CobolValue result;

		if (valueIntervals.isEmpty()) {
			result = null;
		} else {
			final ValueInterval valueInterval = valueIntervals.get(0);

			if (valueInterval.getToValueStmt() == null) {
				final ValueStmt fromValueStmt = valueInterval.getFromValueStmt();
				result = getValue(fromValueStmt, valueProvider);
			} else {
				result = null;
			}
		}

		return result;
	}

	@Override
	public CobolValue getValue(final ValueStmt valueStmt, final CobolValueProvider valueProvider) {
		final CobolValue result;

		if (valueStmt instanceof AndOrCondition) {
			result = getValue((AndOrCondition) valueStmt, valueProvider);
		} else if (valueStmt instanceof ArithmeticComparison) {
			result = getValue((ArithmeticComparison) valueStmt, valueProvider);
		} else if (valueStmt instanceof ArithmeticValueStmt) {
			result = getValue((ArithmeticValueStmt) valueStmt, valueProvider);
		} else if (valueStmt instanceof Basis) {
			result = getValue((Basis) valueStmt, valueProvider);
		} else if (valueStmt instanceof BooleanLiteralValueStmt) {
			result = getValue((BooleanLiteralValueStmt) valueStmt, valueProvider);
		} else if (valueStmt instanceof CallValueStmt) {
			result = getValue((CallValueStmt) valueStmt, valueProvider);
		} else if (valueStmt instanceof CombinableCondition) {
			result = getValue((CombinableCondition) valueStmt, valueProvider);
		} else if (valueStmt instanceof CombinedComparison) {
			result = getValue((CombinedComparison) valueStmt, valueProvider);
		} else if (valueStmt instanceof ConditionValueStmt) {
			result = getValue((ConditionValueStmt) valueStmt, valueProvider);
		} else if (valueStmt instanceof ConditionNameReference) {
			result = getValue((ConditionNameReference) valueStmt, valueProvider);
		} else if (valueStmt instanceof IntegerLiteralValueStmt) {
			result = getValue((IntegerLiteralValueStmt) valueStmt, valueProvider);
		} else if (valueStmt instanceof LiteralValueStmt) {
			result = getValue((LiteralValueStmt) valueStmt, valueProvider);
		} else if (valueStmt instanceof MultDiv) {
			result = getValue((MultDiv) valueStmt, valueProvider);
		} else if (valueStmt instanceof MultDivs) {
			result = getValue((MultDivs) valueStmt, valueProvider);
		} else if (valueStmt instanceof PlusMinus) {
			result = getValue((PlusMinus) valueStmt, valueProvider);
		} else if (valueStmt instanceof Power) {
			result = getValue((Power) valueStmt, valueProvider);
		} else if (valueStmt instanceof Powers) {
			result = getValue((Powers) valueStmt, valueProvider);
		} else if (valueStmt instanceof RelationConditionValueStmt) {
			result = getValue((RelationConditionValueStmt) valueStmt, valueProvider);
		} else if (valueStmt instanceof SignCondition) {
			result = getValue((SignCondition) valueStmt, valueProvider);
		} else if (valueStmt instanceof SimpleCondition) {
			result = getValue((SimpleCondition) valueStmt, valueProvider);
		} else {
			result = null;
		}

		return result;
	}

	@Override
	public CobolValue getValueClauseValue(final DataDescriptionEntry dataDescriptionEntry,
			final CobolValueProvider valueProvider) {
		final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry.getDataDescriptionEntryType();
		final CobolValue result;

		switch (dataDescriptionEntryType) {
		case SCALAR:
		case GROUP:
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
			result = getValueClauseValue(dataDescriptionEntryGroup, valueProvider);
			break;
		case CONDITION:
			final DataDescriptionEntryCondition dataDescriptionEntryCondition = (DataDescriptionEntryCondition) dataDescriptionEntry;
			result = getValueClauseValue(dataDescriptionEntryCondition, valueProvider);
			break;
		case RENAME:
		default:
			result = null;
			break;
		}

		return result;
	}

	private CobolValue getValueClauseValue(final DataDescriptionEntryCondition dataDescriptionEntryCondition,
			final CobolValueProvider valueProvider) {
		final ValueClause valueClause = dataDescriptionEntryCondition.getValueClause();
		return valueClause == null ? null : getValue(valueClause, valueProvider);
	}

	private CobolValue getValueClauseValue(final DataDescriptionEntryGroup dataDescriptionEntryGroup,
			final CobolValueProvider valueProvider) {
		final PictureClause pictureClause = dataDescriptionEntryGroup.getPictureClause();
		final ValueClause valueClause = dataDescriptionEntryGroup.getValueClause();

		final Integer stringLength = pictureClause == null ? null
				: cobolPictureLengthService.getStringLength(pictureClause.getPictureString());
		final CobolValue valueValue = valueClause == null ? null : getValue(valueClause, valueProvider);

		final CobolTypeEnum type = typeService.getType(dataDescriptionEntryGroup);
		final CobolValue result;

		if (type == null) {
			result = null;
		} else {
			switch (type) {
			case BOOLEAN:
				if (valueValue != null) {
					result = valueValue;
				} else {
					result = CobolBooleanValueImpl.FALSE;
				}
				break;
			case FLOAT:
			case INTEGER:
				if (valueValue != null) {
					result = valueValue;
				} else {
					result = CobolDecimalValueImpl.of(BigDecimal.ZERO);
				}
				break;
			case STRING:
				final String stringValue = valueValue == null ? "" : valueService.getString(valueValue);

				if (stringLength == null) {
					result = CobolStringValueImpl.of(stringValue);
				} else {
					result = CobolStringValueImpl.of(StringUtils.rightPad(stringValue, stringLength));
				}
				break;
			case DATA_DESCRIPTION_GROUP:
			default:
				result = null;
				break;
			}
		}

		return result;
	}
}
