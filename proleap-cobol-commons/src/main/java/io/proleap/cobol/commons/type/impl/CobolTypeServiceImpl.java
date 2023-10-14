package io.proleap.cobol.commons.type.impl;

import java.util.List;
import java.util.regex.Matcher;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.BooleanLiteral;
import io.proleap.cobol.asg.metamodel.FigurativeConstant;
import io.proleap.cobol.asg.metamodel.FigurativeConstant.FigurativeConstantType;
import io.proleap.cobol.asg.metamodel.IntegerLiteral;
import io.proleap.cobol.asg.metamodel.Literal;
import io.proleap.cobol.asg.metamodel.Literal.LiteralType;
import io.proleap.cobol.asg.metamodel.NumericLiteral;
import io.proleap.cobol.asg.metamodel.NumericLiteral.NumericLiteralType;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.DataDescriptionEntryCall;
import io.proleap.cobol.asg.metamodel.call.SpecialRegisterCall;
import io.proleap.cobol.asg.metamodel.call.SpecialRegisterCall.SpecialRegisterType;
import io.proleap.cobol.asg.metamodel.call.TableCall;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.PictureClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.ValueClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.ValueInterval;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.CallValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.ConditionValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.IntegerLiteralValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.LiteralValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt.RelationConditionType;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Basis;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDiv;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDivs;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.PlusMinus;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Power;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Powers;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.SimpleCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.SimpleCondition.SimpleConditionType;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.ArithmeticComparison;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.CombinedComparison;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.CombinedCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.RelationalOperator;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.SignCondition;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.type.CobolTypeEnum;
import io.proleap.cobol.commons.type.CobolTypeService;
import io.proleap.cobol.commons.util.CobolPictureParseUtils;

@Singleton
public class CobolTypeServiceImpl implements CobolTypeService {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	public CobolTypeEnum getType(final ArithmeticComparison arithmeticComparison) {
		return CobolTypeEnum.BOOLEAN;
	}

	public CobolTypeEnum getType(final ArithmeticValueStmt arithmeticValueStmt) {
		return getType(arithmeticValueStmt.getMultDivs());
	}

	public CobolTypeEnum getType(final Basis basis) {
		return getType(basis.getBasisValueStmt());
	}

	public CobolTypeEnum getType(final BooleanLiteral booleanLiteral) {
		return CobolTypeEnum.BOOLEAN;
	}

	@Override
	public CobolTypeEnum getType(final Call call) {
		final CallType callType = call.getCallType();
		final CobolTypeEnum result;

		switch (callType) {
		case DATA_DESCRIPTION_ENTRY_CALL:
			result = getType((DataDescriptionEntryCall) call.unwrap());
			break;
		case SPECIAL_REGISTER_CALL:
			result = getType((SpecialRegisterCall) call.unwrap());
			break;
		case TABLE_CALL:
			result = getType((TableCall) call.unwrap());
			break;
		default:
			result = null;
			break;
		}

		return result;
	}

	public CobolTypeEnum getType(final CallValueStmt callValueStmt) {
		return getType(callValueStmt.getCall());
	}

	public CobolTypeEnum getType(final CombinedComparison combinedComparison) {
		return CobolTypeEnum.BOOLEAN;
	}

	public CobolTypeEnum getType(final CombinedCondition combinedCondition) {
		return CobolTypeEnum.BOOLEAN;
	}

	public CobolTypeEnum getType(final ConditionValueStmt conditionValueStmt) {
		return getType(conditionValueStmt.getCombinableCondition());
	}

	@Override
	public CobolTypeEnum getType(final DataDescriptionEntry dataDescriptionEntry) {
		final CobolTypeEnum result;

		if (dataDescriptionEntry instanceof DataDescriptionEntryGroup) {
			result = getType((DataDescriptionEntryGroup) dataDescriptionEntry);
		} else {
			result = null;
		}

		return result;
	}

	public CobolTypeEnum getType(final DataDescriptionEntryCall dataDescriptionEntryCall) {
		final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryCall.getDataDescriptionEntry();
		return getType(dataDescriptionEntry);
	}

	public CobolTypeEnum getType(final DataDescriptionEntryGroup dataDescriptionEntryGroup) {
		final PictureClause pictureClause = dataDescriptionEntryGroup.getPictureClause();
		final ValueClause valueClause = dataDescriptionEntryGroup.getValueClause();
		final CobolTypeEnum result;

		if (pictureClause != null) {
			result = getType(pictureClause);
		} else if (valueClause != null) {
			result = getType(valueClause);
		} else if (cobolDataDescriptionEntryService.hasChildren(dataDescriptionEntryGroup)) {
			result = CobolTypeEnum.DATA_DESCRIPTION_GROUP;
		} else {
			result = null;
		}

		return result;
	}

	public CobolTypeEnum getType(final FigurativeConstant figurativeConstant) {
		final FigurativeConstantType figurativeConstantType = figurativeConstant.getFigurativeConstantType();
		final CobolTypeEnum result;

		switch (figurativeConstantType) {
		case ALL:
		case HIGH_VALUE:
		case HIGH_VALUES:
		case LOW_VALUE:
		case LOW_VALUES:
			result = null;
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
			result = CobolTypeEnum.STRING;
			break;
		case ZERO:
		case ZEROES:
		case ZEROS:
			result = CobolTypeEnum.INTEGER;
			break;
		default:
			result = null;
			break;
		}

		return result;
	}

	public CobolTypeEnum getType(final IntegerLiteral integerLiteral) {
		return CobolTypeEnum.INTEGER;
	}

	public CobolTypeEnum getType(final IntegerLiteralValueStmt integerLiteralValueStmt) {
		return getType(integerLiteralValueStmt.getLiteral());
	}

	public CobolTypeEnum getType(final Literal literal) {
		final LiteralType literalType = literal.getLiteralType();
		final CobolTypeEnum result;

		switch (literalType) {
		case BOOLEAN:
			result = getType(literal.getBooleanLiteral());
			break;
		case CICS_DFH_RESP:
			result = null;
			break;
		case CICS_DFH_VALUE:
			result = null;
			break;
		case FIGURATIVE_CONSTANT:
			result = getType(literal.getFigurativeConstant());
			break;
		case NON_NUMERIC:
			result = CobolTypeEnum.STRING;
			break;
		case NUMERIC:
			result = getType(literal.getNumericLiteral());
			break;
		default:
			result = null;
			break;
		}

		return result;
	}

	public CobolTypeEnum getType(final LiteralValueStmt literalValueStmt) {
		return getType(literalValueStmt.getLiteral());
	}

	public CobolTypeEnum getType(final MultDiv multDiv) {
		return getType(multDiv.getPowers());
	}

	public CobolTypeEnum getType(final MultDivs multDivs) {
		return getType(multDivs.getPowers());
	}

	public CobolTypeEnum getType(final NumericLiteral numericLiteral) {
		final NumericLiteralType numericLiteralType = numericLiteral.getNumericLiteralType();
		final CobolTypeEnum result;

		switch (numericLiteralType) {
		case INTEGER:
			result = CobolTypeEnum.INTEGER;
			break;
		case FLOAT:
		default:
			result = CobolTypeEnum.FLOAT;
			break;
		}

		return result;
	}

	@Override
	public CobolTypeEnum getType(final PictureClause pictureClause) {
		final String pictureString = pictureClause.getPictureString();
		return getType(pictureString);
	}

	public CobolTypeEnum getType(final PlusMinus plusMinus) {
		return getType(plusMinus.getMultDivs());
	}

	public CobolTypeEnum getType(final Power power) {
		return getType(power.getBasis());
	}

	public CobolTypeEnum getType(final Powers powers) {
		return getType(powers.getBasis());
	}

	public CobolTypeEnum getType(final RelationalOperator relationalOperator) {
		return CobolTypeEnum.BOOLEAN;
	}

	public CobolTypeEnum getType(final RelationConditionValueStmt relationConditionValueStmt) {
		final RelationConditionType relationConditionType = relationConditionValueStmt.getRelationConditionType();
		final CobolTypeEnum result;

		switch (relationConditionType) {
		case ARITHMETIC:
			result = getType(relationConditionValueStmt.getArithmeticComparison());
			break;
		case COMBINED:
			result = getType(relationConditionValueStmt.getCombinedComparison());
			break;
		case SIGN:
			result = getType(relationConditionValueStmt.getSignCondition());
			break;
		default:
			result = null;
			break;

		}

		return result;
	}

	public CobolTypeEnum getType(final SignCondition signCondition) {
		return CobolTypeEnum.BOOLEAN;
	}

	public CobolTypeEnum getType(final SimpleCondition simpleCondition) {
		final SimpleConditionType simpleConditionType = simpleCondition.getSimpleConditionType();
		final CobolTypeEnum result;

		switch (simpleConditionType) {
		case CLASS_CONDITION:
			result = getType(simpleCondition.getClassCondition());
			break;
		case CONDITION:
			result = getType(simpleCondition.getCondition());
			break;
		case CONDITION_NAME_REFERENCE:
			result = getType(simpleCondition.getConditionNameReference());
			break;
		case RELATION_CONDITION:
			result = getType(simpleCondition.getRelationCondition());
			break;
		default:
			result = null;
			break;
		}

		return result;
	}

	public CobolTypeEnum getType(final SpecialRegisterCall specialRegisterCall) {
		final SpecialRegisterType type = specialRegisterCall.getSpecialRegisterType();
		final CobolTypeEnum result;

		switch (type) {
		case ADDRESS_OF:
			result = CobolTypeEnum.INTEGER;
			break;
		case LENGTH_OF:
			result = CobolTypeEnum.INTEGER;
			break;
		case LINAGE_COUNTER:
			result = CobolTypeEnum.INTEGER;
			break;
		case LINE_COUNTER:
			result = CobolTypeEnum.INTEGER;
			break;
		case PAGE_COUNTER:
			result = CobolTypeEnum.INTEGER;
			break;
		case SORT_FILE_SIZE:
			result = CobolTypeEnum.INTEGER;
			break;
		default:
			result = null;
			break;
		}

		return result;
	}

	public CobolTypeEnum getType(final String pictureString) {
		final Matcher matcher9 = CobolPictureParseUtils.PATTERN_9.matcher(pictureString);
		final Matcher matcher9Length = CobolPictureParseUtils.PATTERN_9Length.matcher(pictureString);
		final Matcher matcher9LengthV9 = CobolPictureParseUtils.PATTERN_9LengthV9.matcher(pictureString);
		final Matcher matcher9V9Length = CobolPictureParseUtils.PATTERN_9V9Length.matcher(pictureString);
		final Matcher matcher9LengthV9Length = CobolPictureParseUtils.PATTERN_9LengthV9Length.matcher(pictureString);
		final Matcher matcher9DOT9 = CobolPictureParseUtils.PATTERN_9DOT9.matcher(pictureString);
		final Matcher matcher9V9 = CobolPictureParseUtils.PATTERN_9V9.matcher(pictureString);
		final Matcher matcherS9 = CobolPictureParseUtils.PATTERN_S9.matcher(pictureString);
		final Matcher matcherS9Length = CobolPictureParseUtils.PATTERN_S9Length.matcher(pictureString);
		final Matcher matcherS9LengthV9 = CobolPictureParseUtils.PATTERN_S9LengthV9.matcher(pictureString);
		final Matcher matcherS9V9Length = CobolPictureParseUtils.PATTERN_S9V9Length.matcher(pictureString);
		final Matcher matcherS9LengthV9Length = CobolPictureParseUtils.PATTERN_S9LengthV9Length.matcher(pictureString);
		final Matcher matcherS9V9 = CobolPictureParseUtils.PATTERN_S9V9.matcher(pictureString);
		final Matcher matcherX = CobolPictureParseUtils.PATTERN_X.matcher(pictureString);
		final Matcher matcherXLength = CobolPictureParseUtils.PATTERN_XLength.matcher(pictureString);
		final Matcher matcherA = CobolPictureParseUtils.PATTERN_A.matcher(pictureString);
		final Matcher matcherALength = CobolPictureParseUtils.PATTERN_ALength.matcher(pictureString);

		final CobolTypeEnum result;

		if (matcher9.matches()) {
			result = CobolTypeEnum.INTEGER;
		} else if (matcher9Length.matches()) {
			result = CobolTypeEnum.INTEGER;
		} else if (matcher9V9.matches()) {
			result = CobolTypeEnum.FLOAT;
		} else if (matcher9DOT9.matches()) {
			result = CobolTypeEnum.FLOAT;
		} else if (matcher9LengthV9.matches()) {
			result = CobolTypeEnum.FLOAT;
		} else if (matcher9V9Length.matches()) {
			result = CobolTypeEnum.FLOAT;
		} else if (matcher9LengthV9Length.matches()) {
			result = CobolTypeEnum.FLOAT;
		} else if (matcherS9.matches()) {
			result = CobolTypeEnum.INTEGER;
		} else if (matcherS9Length.matches()) {
			result = CobolTypeEnum.INTEGER;
		} else if (matcherX.matches()) {
			result = CobolTypeEnum.STRING;
		} else if (matcherXLength.matches()) {
			result = CobolTypeEnum.STRING;
		} else if (matcherS9LengthV9.matches()) {
			result = CobolTypeEnum.FLOAT;
		} else if (matcherS9V9Length.matches()) {
			result = CobolTypeEnum.FLOAT;
		} else if (matcherS9LengthV9Length.matches()) {
			result = CobolTypeEnum.FLOAT;
		} else if (matcherS9V9.matches()) {
			result = CobolTypeEnum.FLOAT;
		} else if (matcherA.matches()) {
			result = CobolTypeEnum.STRING;
		} else if (matcherALength.matches()) {
			result = CobolTypeEnum.STRING;
		} else {
			result = null;
		}

		return result;
	}

	public CobolTypeEnum getType(final TableCall tableCall) {
		final DataDescriptionEntry dataDescriptionEntry = tableCall.getDataDescriptionEntry();
		return getType(dataDescriptionEntry);
	}

	@Override
	public CobolTypeEnum getType(final ValueClause valueClause) {
		final List<ValueInterval> valueIntervals = valueClause.getValueIntervals();
		final ValueInterval valueInterval = valueIntervals.get(0);
		return getType(valueInterval.getFromValueStmt());
	}

	@Override
	public CobolTypeEnum getType(final ValueStmt valueStmt) {
		final CobolTypeEnum result;

		if (valueStmt instanceof ArithmeticComparison) {
			result = getType((ArithmeticComparison) valueStmt);
		} else if (valueStmt instanceof ArithmeticValueStmt) {
			result = getType((ArithmeticValueStmt) valueStmt);
		} else if (valueStmt instanceof Basis) {
			result = getType((Basis) valueStmt);
		} else if (valueStmt instanceof CallValueStmt) {
			result = getType((CallValueStmt) valueStmt);
		} else if (valueStmt instanceof CombinedComparison) {
			result = getType((CombinedComparison) valueStmt);
		} else if (valueStmt instanceof CombinedCondition) {
			result = getType((CombinedCondition) valueStmt);
		} else if (valueStmt instanceof ConditionValueStmt) {
			result = getType((ConditionValueStmt) valueStmt);
		} else if (valueStmt instanceof IntegerLiteralValueStmt) {
			result = getType((IntegerLiteralValueStmt) valueStmt);
		} else if (valueStmt instanceof LiteralValueStmt) {
			result = getType((LiteralValueStmt) valueStmt);
		} else if (valueStmt instanceof MultDiv) {
			result = getType((MultDiv) valueStmt);
		} else if (valueStmt instanceof MultDivs) {
			result = getType((MultDivs) valueStmt);
		} else if (valueStmt instanceof PlusMinus) {
			result = getType((PlusMinus) valueStmt);
		} else if (valueStmt instanceof Power) {
			result = getType((Power) valueStmt);
		} else if (valueStmt instanceof Powers) {
			result = getType((Powers) valueStmt);
		} else if (valueStmt instanceof RelationConditionValueStmt) {
			result = getType((RelationConditionValueStmt) valueStmt);
		} else if (valueStmt instanceof RelationalOperator) {
			result = getType((RelationalOperator) valueStmt);
		} else if (valueStmt instanceof SimpleCondition) {
			result = getType((SimpleCondition) valueStmt);
		} else if (valueStmt instanceof SignCondition) {
			result = getType((SignCondition) valueStmt);
		} else {
			result = null;
		}

		return result;
	}
}
