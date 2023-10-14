package io.proleap.cobol.transform.java.expression.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.ArithmeticComparison;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.RelationalOperator;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.RelationalOperator.RelationalOperatorType;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.type.CobolTypeEnum;
import io.proleap.cobol.commons.type.CobolTypeService;
import io.proleap.cobol.commons.value.CobolValueSpaceService;
import io.proleap.cobol.transform.java.expression.JavaArithmeticExpressionClassifier;

@Singleton
public class JavaArithmeticExpressionClassifierImpl implements JavaArithmeticExpressionClassifier {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Inject
	private CobolTypeService cobolTypeService;

	@Inject
	private CobolValueSpaceService cobolValueSpaceService;

	@Override
	public JavaArithmeticExpressionTypeEnum classify(final ArithmeticComparison arithmeticComparison) {
		final RelationalOperator operator = arithmeticComparison.getOperator();
		final RelationalOperatorType relationalOperatorType = operator.getRelationalOperatorType();
		final ArithmeticValueStmt arithmeticExpressionLeft = arithmeticComparison.getArithmeticExpressionLeft();
		final ArithmeticValueStmt arithmeticExpressionRight = arithmeticComparison.getArithmeticExpressionRight();

		final JavaArithmeticExpressionTypeEnum result;

		if (isComparisonBetweenGroupAndString(relationalOperatorType, arithmeticExpressionLeft,
				arithmeticExpressionRight)) {
			result = JavaArithmeticExpressionTypeEnum.COMPARISON_BETWEEN_GROUP_AND_STRING;
		} else if (isComparisonBetweenStringAndBlank(relationalOperatorType, arithmeticExpressionLeft,
				arithmeticExpressionRight)) {
			result = JavaArithmeticExpressionTypeEnum.COMPARISON_BETWEEN_STRING_AND_BLANK;
		} else {
			result = JavaArithmeticExpressionTypeEnum.DEFAULT;
		}

		return result;
	}

	protected boolean isComparisonBetweenGroupAndString(final RelationalOperatorType relationalOperatorType,
			final ArithmeticValueStmt arithmeticExpressionLeft, final ArithmeticValueStmt arithmeticExpressionRight) {
		final boolean result;

		if (RelationalOperatorType.EQUAL.equals(relationalOperatorType)
				|| RelationalOperatorType.NOT_EQUAL.equals(relationalOperatorType)) {
			final CobolTypeEnum type = cobolTypeService.getType(arithmeticExpressionLeft);

			if (CobolTypeEnum.DATA_DESCRIPTION_GROUP.equals(type)) {
				final DataDescriptionEntry dataDescriptionEntry = cobolDataDescriptionEntryService
						.getDataDescriptionEntry(arithmeticExpressionLeft);
				final DataDescriptionEntryGroup typeGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
				final boolean hasDataDescriptionEntries = !typeGroup.getDataDescriptionEntries().isEmpty();

				if (hasDataDescriptionEntries) {
					result = cobolValueSpaceService.isSpace(arithmeticExpressionRight);
				} else {
					result = false;
				}
			} else {
				result = false;
			}
		} else {
			result = false;
		}

		return result;
	}

	protected boolean isComparisonBetweenStringAndBlank(final RelationalOperatorType relationalOperatorType,
			final ArithmeticValueStmt arithmeticExpressionLeft, final ArithmeticValueStmt arithmeticExpressionRight) {
		final boolean result;

		if (RelationalOperatorType.EQUAL.equals(relationalOperatorType)
				|| RelationalOperatorType.NOT_EQUAL.equals(relationalOperatorType)) {
			final boolean receivingTypeIsString = CobolTypeEnum.STRING
					.equals(cobolTypeService.getType(arithmeticExpressionLeft));

			if (receivingTypeIsString) {
				result = cobolValueSpaceService.isSpace(arithmeticExpressionRight);
			} else {
				result = false;
			}
		} else {
			result = false;
		}

		return result;
	}
}
