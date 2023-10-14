package io.proleap.cobol.transform.java.expression.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.DataDescriptionEntryCall;
import io.proleap.cobol.asg.metamodel.call.SpecialRegisterCall;
import io.proleap.cobol.asg.metamodel.call.SpecialRegisterCall.SpecialRegisterType;
import io.proleap.cobol.asg.metamodel.call.TableCall;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.registry.ASGElementRegistry;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.CallValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.ConditionValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt.RelationConditionType;
import io.proleap.cobol.asg.metamodel.valuestmt.Subscript;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Basis;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDiv;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDiv.MultDivType;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDivs;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.PlusMinus;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.PlusMinus.PlusMinusType;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Power;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Powers;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Powers.PowersType;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.AndOrCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.AndOrCondition.AndOrConditionType;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.CombinableCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.ConditionNameReference;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.SimpleCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.SimpleCondition.SimpleConditionType;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.ArithmeticComparison;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.RelationalOperator;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.RelationalOperator.RelationalOperatorType;
import io.proleap.cobol.asg.util.ANTLRUtils;
import io.proleap.cobol.commons.datadescription.CobolPictureStringService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.transform.java.expression.JavaArithmeticExpressionClassifier;
import io.proleap.cobol.transform.java.expression.JavaArithmeticExpressionClassifier.JavaArithmeticExpressionTypeEnum;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.java.identifier.JavaIdentifierService;
import io.proleap.cobol.transform.java.identifier.variable.JavaFileDescriptionEntryIdentifierService;
import io.proleap.cobol.transform.java.identifier.variable.JavaVariableIdentifierService;
import io.proleap.cobol.transform.java.util.JavaLiteralUtils;

@Singleton
public class JavaExpressionServiceImpl implements JavaExpressionService {

	private static final String DOT = ".";

	@Inject
	private JavaArithmeticExpressionClassifier javaArithmeticExpressionClassifier;

	@Inject
	private JavaFileDescriptionEntryIdentifierService javaFileDescriptionEntryIdentifierService;

	@Inject
	private JavaIdentifierService javaIdentifierService;

	@Inject
	private JavaVariableIdentifierService javaVariableIdentifierService;

	@Inject
	private CobolPictureStringService pictureStringService;

	@Inject
	private CobolValueService valueService;

	@Inject
	private CobolValueStmtService valueStmtService;

	protected List<DataDescriptionEntry> collectCallHierarchy(final DataDescriptionEntry dataDescriptionEntry) {
		final List<DataDescriptionEntry> result = new ArrayList<DataDescriptionEntry>();
		DataDescriptionEntry currentDataDescriptionEntry = dataDescriptionEntry;

		do {
			result.add(currentDataDescriptionEntry);
			currentDataDescriptionEntry = currentDataDescriptionEntry.getParentDataDescriptionEntryGroup();
		} while (currentDataDescriptionEntry != null);

		return new ArrayList<>(result);
	}

	@Override
	public String mapToCall(final Call call) {
		final CallType callType = call.getCallType();
		final String result;

		switch (callType) {
		case DATA_DESCRIPTION_ENTRY_CALL:
			final DataDescriptionEntryCall dataDescriptionEntryCall = (DataDescriptionEntryCall) call.unwrap();
			result = mapToCall(dataDescriptionEntryCall);
			break;
		case TABLE_CALL:
			final TableCall tableCall = (TableCall) call.unwrap();
			result = mapToCall(tableCall);
			break;
		case SPECIAL_REGISTER_CALL:
			final SpecialRegisterCall specialRegisterCall = (SpecialRegisterCall) call.unwrap();
			result = mapToCall(specialRegisterCall);
			break;
		default:
			result = javaIdentifierService.mapToIdentifier(call.getName());
			break;
		}

		return result;
	}

	@Override
	public String mapToCall(final DataDescriptionEntryCall dataDescriptionEntryCall) {
		final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryCall.getDataDescriptionEntry();

		final Program program = dataDescriptionEntry.getProgram();
		final ASGElementRegistry asgElementRegistry = program.getASGElementRegistry();

		final List<DataDescriptionEntry> callHierarchy = collectCallHierarchy(dataDescriptionEntry);
		final StringBuffer result = new StringBuffer();
		boolean isFirst = true;

		final FileDescriptionEntry fileDescriptionEntry = (FileDescriptionEntry) ANTLRUtils
				.findParent(FileDescriptionEntry.class, dataDescriptionEntry.getCtx(), asgElementRegistry);

		if (fileDescriptionEntry != null) {
			result.append(mapToCall(fileDescriptionEntry));
			isFirst = false;
		}

		for (final DataDescriptionEntry currentDataDescriptionEntry : callHierarchy) {
			if (!isFirst) {
				result.append(DOT);
			}

			final String identifier = javaVariableIdentifierService.mapToIdentifier(currentDataDescriptionEntry);
			result.append(identifier);

			isFirst = false;
		}

		return result.toString();
	}

	@Override
	public String mapToCall(final FileDescriptionEntry fileDescriptionEntry) {
		return javaFileDescriptionEntryIdentifierService.mapToIdentifier(fileDescriptionEntry);
	}

	@Override
	public String mapToCall(final SpecialRegisterCall specialRegisterCall) {
		final SpecialRegisterType type = specialRegisterCall.getSpecialRegisterType();
		final StringBuffer result = new StringBuffer();

		switch (type) {
		case ADDRESS_OF:
			result.append("entityService.getAddress(");
			result.append(mapToCall(specialRegisterCall.getIdentifierCall()));
			result.append(")");
			break;
		case LENGTH_OF:
			result.append("entityService.getLength(");
			result.append(mapToCall(specialRegisterCall.getIdentifierCall()));
			result.append(")");
			break;
		default:
			break;
		}

		return result.toString();
	}

	@Override
	public String mapToCall(final TableCall tableCall) {
		final List<Subscript> subscripts = tableCall.getSubscripts();
		final Iterator<Subscript> subscriptIterator = subscripts.iterator();
		final List<DataDescriptionEntry> callHierarchy = collectCallHierarchy(tableCall.getDataDescriptionEntry());

		final StringBuffer result = new StringBuffer();
		boolean isFirst = true;

		for (final DataDescriptionEntry dataDescriptionEntry : callHierarchy) {
			if (!isFirst) {
				result.append(DOT);
			}

			result.append(javaVariableIdentifierService.mapToIdentifier(dataDescriptionEntry));

			final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry
					.getDataDescriptionEntryType();

			if (DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)) {
				final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
				final int numberOfInstances = pictureStringService.getMaxOccurs(dataDescriptionEntryGroup);

				if (subscriptIterator.hasNext() && numberOfInstances > 1) {
					result.append(".get(");

					final Subscript subscript = subscriptIterator.next();
					final ValueStmt subscriptValueStmt = subscript.getSubscriptValueStmt();

					final CobolValue subscriptValue = valueStmtService.getValue(subscriptValueStmt, null);
					final BigDecimal decimalSubscriptValue = valueService.getDecimal(subscriptValue);

					if (decimalSubscriptValue != null) {
						result.append(String.format("%d", decimalSubscriptValue.intValue() - 1));
					} else {
						result.append(mapToExpression(subscriptValueStmt));
						result.append(".intValue() - 1");
					}

					result.append(")");
				}
			}

			isFirst = false;
		}

		return result.toString();
	}

	@Override
	public String mapToExpression(final AndOrCondition andOrCondition) {
		final Boolean value = valueService.getBoolean(valueStmtService.getValue(andOrCondition, null));
		final StringBuffer result = new StringBuffer();

		if (value != null) {
			result.append(JavaLiteralUtils.mapToLiteral(value));
		} else {
			final AndOrConditionType type = andOrCondition.getAndOrConditionType();

			switch (type) {
			case AND:
				result.append("&&");
				break;
			case OR:
				result.append("||");
				break;
			default:
				break;
			}

			if (andOrCondition.getCombinableCondition() != null) {
				result.append(mapToExpression(andOrCondition.getCombinableCondition()));
			}
		}

		return result.toString();
	}

	@Override
	public String mapToExpression(final ArithmeticComparison arithmeticComparison) {
		final Boolean value = valueService.getBoolean(valueStmtService.getValue(arithmeticComparison, null));
		final StringBuffer result = new StringBuffer();

		if (value != null) {
			result.append(JavaLiteralUtils.mapToLiteral(value));
		} else {
			final RelationalOperator operator = arithmeticComparison.getOperator();
			final RelationalOperatorType relationalOperatorType = operator.getRelationalOperatorType();

			final ArithmeticValueStmt arithmeticExpressionLeft = arithmeticComparison.getArithmeticExpressionLeft();
			final ArithmeticValueStmt arithmeticExpressionRight = arithmeticComparison.getArithmeticExpressionRight();

			final JavaArithmeticExpressionTypeEnum arithmeticExpressionType = javaArithmeticExpressionClassifier
					.classify(arithmeticComparison);

			switch (arithmeticExpressionType) {
			case COMPARISON_BETWEEN_STRING_AND_BLANK:
				switch (relationalOperatorType) {
				case NOT_EQUAL:
					result.append("!");
					break;
				default:
					break;
				}

				result.append("Strings.isNullOrEmpty(");
				result.append(mapToExpression(arithmeticComparison.getArithmeticExpressionLeft()));
				result.append(")");
				break;
			case COMPARISON_BETWEEN_GROUP_AND_STRING:
				switch (relationalOperatorType) {
				case NOT_EQUAL:
					result.append("!");
					break;
				default:
					break;
				}

				result.append("entityService.isEmpty(");
				result.append(mapToExpression(arithmeticComparison.getArithmeticExpressionLeft()));
				result.append(")");
				break;
			default:
				result.append(mapToExpression(arithmeticExpressionLeft));
				result.append(".compareTo(");
				result.append(mapToExpression(arithmeticExpressionRight));
				result.append(") ");

				switch (relationalOperatorType) {
				case GREATER:
					result.append(">");
					break;
				case GREATER_OR_EQUAL:
					result.append(">=");
					break;
				case LESS:
					result.append("<");
					break;
				case LESS_OR_EQUAL:
					result.append("<=");
					break;
				case EQUAL:
					result.append("==");
					break;
				case NOT_EQUAL:
					result.append("!=");
					break;
				default:
					break;
				}

				result.append(" 0");
				break;
			}
		}

		return result.toString();
	}

	@Override
	public String mapToExpression(final ArithmeticValueStmt arithmeticValueStmt) {
		final BigDecimal value = valueService.getDecimal(valueStmtService.getValue(arithmeticValueStmt, null));
		final StringBuffer result = new StringBuffer();

		if (value != null) {
			result.append(JavaLiteralUtils.mapToLiteral(value));
		} else {
			result.append(mapToExpression(arithmeticValueStmt.getMultDivs()));

			for (final PlusMinus plusMinus : arithmeticValueStmt.getPlusMinus()) {
				result.append(mapToExpression(plusMinus));
			}
		}

		return result.toString();
	}

	@Override
	public String mapToExpression(final Basis basis) {
		final BigDecimal value = valueService.getDecimal(valueStmtService.getValue(basis, null));
		final String result;

		if (value != null) {
			result = JavaLiteralUtils.mapToLiteral(value);
		} else {
			result = mapToExpression(basis.getBasisValueStmt());
		}

		return result;
	}

	@Override
	public String mapToExpression(final CallValueStmt callValueStmt) {
		final Call call = callValueStmt.getCall();
		return mapToCall(call);
	}

	@Override
	public String mapToExpression(final CombinableCondition combinableCondition) {
		final Boolean value = valueService.getBoolean(valueStmtService.getValue(combinableCondition, null));
		final StringBuffer result = new StringBuffer();

		if (value != null) {
			result.append(JavaLiteralUtils.mapToLiteral(value));
		} else {
			if (combinableCondition.isNot()) {
				result.append("!");
			}

			result.append(mapToExpression(combinableCondition.getSimpleCondition()));
		}

		return result.toString();
	}

	@Override
	public String mapToExpression(final ConditionNameReference conditionNameReference) {
		final Call conditionCall = conditionNameReference.getConditionCall();
		return mapToCall(conditionCall);
	}

	@Override
	public String mapToExpression(final ConditionValueStmt conditionValueStmt) {
		final Boolean value = valueService.getBoolean(valueStmtService.getValue(conditionValueStmt, null));
		final StringBuffer result = new StringBuffer();

		if (value != null) {
			result.append(JavaLiteralUtils.mapToLiteral(value));
		} else {
			result.append(mapToExpression(conditionValueStmt.getCombinableCondition()));

			for (final AndOrCondition andOrCondition : conditionValueStmt.getAndOrConditions()) {
				result.append(mapToExpression(andOrCondition));
			}
		}

		return result.toString();
	}

	@Override
	public String mapToExpression(final MultDiv multDiv) {
		final BigDecimal value = valueService.getDecimal(valueStmtService.getValue(multDiv, null));
		final StringBuffer result = new StringBuffer();

		if (value != null) {
			result.append(JavaLiteralUtils.mapToLiteral(value));
		} else {
			final MultDivType type = multDiv.getMultDivType();

			switch (type) {
			case DIV:
				result.append("/");
				break;
			case MULT:
				result.append("*");
				break;
			default:
				break;
			}

			result.append(mapToExpression(multDiv.getPowers()));
		}

		return result.toString();
	}

	@Override
	public String mapToExpression(final MultDivs multDivs) {
		final BigDecimal value = valueService.getDecimal(valueStmtService.getValue(multDivs, null));
		final StringBuffer result = new StringBuffer();

		if (value != null) {
			result.append(JavaLiteralUtils.mapToLiteral(value));
		} else {
			result.append(mapToExpression(multDivs.getPowers()));

			for (final MultDiv multDiv : multDivs.getMultDivs()) {
				result.append(mapToExpression(multDiv));
			}
		}

		return result.toString();
	}

	@Override
	public String mapToExpression(final PlusMinus plusMinus) {
		final BigDecimal value = valueService.getDecimal(valueStmtService.getValue(plusMinus, null));
		final StringBuffer result = new StringBuffer();

		if (value != null) {
			result.append(JavaLiteralUtils.mapToLiteral(value));
		} else {
			final PlusMinusType type = plusMinus.getPlusMinusType();

			switch (type) {
			case MINUS:
				result.append("-");
				break;
			case PLUS:
				break;
			default:
				break;
			}

			result.append(mapToExpression(plusMinus.getMultDivs()));
		}

		return result.toString();
	}

	@Override
	public String mapToExpression(final Powers powers) {
		final BigDecimal value = valueService.getDecimal(valueStmtService.getValue(powers, null));
		final StringBuffer result = new StringBuffer();

		if (value != null) {
			result.append(JavaLiteralUtils.mapToLiteral(value));
		} else {
			final PowersType type = powers.getPowersType();

			if (type != null) {
				switch (type) {
				case MINUS:
					result.append("-");
					break;
				case PLUS:
					break;
				default:
					break;
				}
			}

			for (@SuppressWarnings("unused")
			final Power power : powers.getPowers()) {
				result.append("Math.pow(");
			}

			result.append(mapToExpression(powers.getBasis()));

			for (final Power power : powers.getPowers()) {
				result.append(",");
				result.append(mapToExpression(power.getBasis()));
				result.append(")");
			}
		}

		return result.toString();
	}

	@Override
	public String mapToExpression(final RelationConditionValueStmt relationCondition) {
		final Boolean value = valueService.getBoolean(valueStmtService.getValue(relationCondition, null));
		final StringBuffer result = new StringBuffer();

		if (value != null) {
			result.append(JavaLiteralUtils.mapToLiteral(value));
		} else {
			final RelationConditionType type = relationCondition.getRelationConditionType();

			switch (type) {
			case ARITHMETIC:
				result.append(mapToExpression(relationCondition.getArithmeticComparison()));
				break;
			case COMBINED:
				result.append(mapToExpression(relationCondition.getCombinedComparison()));
				break;
			case SIGN:
				result.append(mapToExpression(relationCondition.getSignCondition()));
				break;
			default:
				break;
			}
		}

		return result.toString();
	}

	@Override
	public String mapToExpression(final SimpleCondition simpleCondition) {
		final Boolean value = valueService.getBoolean(valueStmtService.getValue(simpleCondition, null));
		final StringBuffer result = new StringBuffer();

		if (value != null) {
			result.append(JavaLiteralUtils.mapToLiteral(value));
		} else {
			final SimpleConditionType type = simpleCondition.getSimpleConditionType();

			switch (type) {
			case CLASS_CONDITION:
				result.append(mapToExpression(simpleCondition.getClassCondition()));
				break;
			case CONDITION:
				result.append(mapToExpression(simpleCondition.getCondition()));
				break;
			case CONDITION_NAME_REFERENCE:
				result.append(mapToExpression(simpleCondition.getConditionNameReference()));
				break;
			case RELATION_CONDITION:
				result.append(mapToExpression(simpleCondition.getRelationCondition()));
				break;
			default:
				break;
			}
		}

		return result.toString();
	}

	@Override
	public String mapToExpression(final ValueStmt valueStmt) {
		final StringBuffer result = new StringBuffer();
		final CobolValue value = valueStmtService.getValue(valueStmt, null);

		final BigDecimal decimalValue = valueService.getDecimal(value);
		final Boolean booleanValue = valueService.getBoolean(value);
		final String stringValue = valueService.getString(value);

		if (decimalValue != null) {
			result.append(JavaLiteralUtils.mapToLiteral(decimalValue));
		} else if (booleanValue != null) {
			result.append(JavaLiteralUtils.mapToLiteral(booleanValue));
		} else if (stringValue != null) {
			result.append(JavaLiteralUtils.mapToLiteral(stringValue));
		} else if (valueStmt instanceof AndOrCondition) {
			result.append(mapToExpression((AndOrCondition) valueStmt));
		} else if (valueStmt instanceof ArithmeticComparison) {
			result.append(mapToExpression((ArithmeticComparison) valueStmt));
		} else if (valueStmt instanceof Basis) {
			result.append(mapToExpression((Basis) valueStmt));
		} else if (valueStmt instanceof CallValueStmt) {
			result.append(mapToExpression((CallValueStmt) valueStmt));
		} else if (valueStmt instanceof CombinableCondition) {
			result.append(mapToExpression((CombinableCondition) valueStmt));
		} else if (valueStmt instanceof ConditionValueStmt) {
			result.append(mapToExpression((ConditionValueStmt) valueStmt));
		} else if (valueStmt instanceof ConditionNameReference) {
			result.append(mapToExpression((ConditionNameReference) valueStmt));
		} else if (valueStmt instanceof MultDiv) {
			result.append(mapToExpression((MultDiv) valueStmt));
		} else if (valueStmt instanceof MultDivs) {
			result.append(mapToExpression((MultDivs) valueStmt));
		} else if (valueStmt instanceof PlusMinus) {
			result.append(mapToExpression((PlusMinus) valueStmt));
		} else if (valueStmt instanceof Powers) {
			result.append(mapToExpression((Powers) valueStmt));
		} else if (valueStmt instanceof RelationConditionValueStmt) {
			result.append(mapToExpression((RelationConditionValueStmt) valueStmt));
		} else if (valueStmt instanceof SimpleCondition) {
			result.append(mapToExpression((SimpleCondition) valueStmt));
		}

		return result.toString();
	}
}
