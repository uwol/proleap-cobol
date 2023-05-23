package io.proleap.cobol.analysis.issues.rules.procedure.condition;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.analysis.util.StatementUtils;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.ConditionValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.SimpleCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.ArithmeticComparison;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.util.CobolStreamUtils;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;

@Singleton
public class IdenticalExpressionsAndOrConditionFeatureGenerator extends FeatureGenerator<Statement> {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Inject
	private CobolValueService valueService;

	@Inject
	private CobolValueStmtService valueStmtService;

	protected boolean areRelevantValues(final ArithmeticComparison leftArithmetic,
			final ArithmeticComparison rightArithmetic) {
		final ArithmeticValueStmt leftConditionLeftExpression = leftArithmetic.getArithmeticExpressionLeft();
		final DataDescriptionEntry dataDescriptionEntryleftLeft = cobolDataDescriptionEntryService
				.getDataDescriptionEntry(leftConditionLeftExpression);
		final ArithmeticValueStmt leftConditionRightExpression = leftArithmetic.getArithmeticExpressionRight();
		final DataDescriptionEntry dataDescriptionEntryleftRight = cobolDataDescriptionEntryService
				.getDataDescriptionEntry(leftConditionRightExpression);
		final ArithmeticValueStmt rightConditionLeftExpression = rightArithmetic.getArithmeticExpressionLeft();
		final DataDescriptionEntry dataDescriptionEntryRightLeft = cobolDataDescriptionEntryService
				.getDataDescriptionEntry(rightConditionLeftExpression);
		final ArithmeticValueStmt rightConditionRightExpression = rightArithmetic.getArithmeticExpressionRight();
		final DataDescriptionEntry dataDescriptionEntryRightRight = cobolDataDescriptionEntryService
				.getDataDescriptionEntry(rightConditionRightExpression);

		final List<Object> leftExpressions = new ArrayList<>();
		final List<Object> rightExpressions = new ArrayList<>();

		if (dataDescriptionEntryleftLeft != null) {
			leftExpressions.add(dataDescriptionEntryleftLeft);
		} else {
			final CobolValue value = valueStmtService.getValue(leftConditionLeftExpression, null);
			leftExpressions.add(valueService.getValue(value));
		}

		if (dataDescriptionEntryleftRight != null) {
			leftExpressions.add(dataDescriptionEntryleftRight);
		} else {
			final CobolValue value = valueStmtService.getValue(leftConditionRightExpression, null);
			leftExpressions.add(valueService.getValue(value));
		}

		if (dataDescriptionEntryRightLeft != null) {
			rightExpressions.add(dataDescriptionEntryRightLeft);
		} else {
			final CobolValue value = valueStmtService.getValue(rightConditionLeftExpression, null);
			rightExpressions.add(valueService.getValue(value));
		}

		if (dataDescriptionEntryRightRight != null) {
			rightExpressions.add(dataDescriptionEntryRightRight);
		} else {
			final CobolValue value = valueStmtService.getValue(rightConditionRightExpression, null);
			rightExpressions.add(valueService.getValue(value));
		}

		return new HashSet<>(leftExpressions).equals(new HashSet<>(rightExpressions));
	}

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit)
				.filter(statement -> StatementUtils.containsCondition(statement))
				.filter(statement -> isRelevantConditionStatement(StatementUtils.getConditions(statement)));
	}

	protected boolean isRelevantConditionStatement(final List<ConditionValueStmt> conditions) {
		boolean result = false;

		for (final ConditionValueStmt condition : conditions) {
			if (condition.getAndOrConditions().size() == 1) {
				final SimpleCondition leftCondition = condition.getCombinableCondition().getSimpleCondition();

				if (leftCondition.getSimpleConditionType()
						.equals(SimpleCondition.SimpleConditionType.RELATION_CONDITION)) {
					final RelationConditionValueStmt leftRelation = leftCondition.getRelationCondition();

					if (leftRelation.getRelationConditionType()
							.equals(RelationConditionValueStmt.RelationConditionType.ARITHMETIC)) {
						if (condition.getAndOrConditions().get(0).getCombinableCondition() != null) {
							final SimpleCondition rightCondition = condition.getAndOrConditions().get(0)
									.getCombinableCondition().getSimpleCondition();

							if (rightCondition.getSimpleConditionType()
									.equals(SimpleCondition.SimpleConditionType.RELATION_CONDITION)) {
								final RelationConditionValueStmt rightRelation = rightCondition.getRelationCondition();

								if (rightRelation.getRelationConditionType()
										.equals(RelationConditionValueStmt.RelationConditionType.ARITHMETIC)) {
									final ArithmeticComparison leftArithmetic = leftRelation.getArithmeticComparison();
									final ArithmeticComparison rightArithmetic = rightRelation
											.getArithmeticComparison();
									result = areRelevantValues(leftArithmetic, rightArithmetic);
								}
							}
						}
					}
				}
			}
		}

		return result;
	}
}
