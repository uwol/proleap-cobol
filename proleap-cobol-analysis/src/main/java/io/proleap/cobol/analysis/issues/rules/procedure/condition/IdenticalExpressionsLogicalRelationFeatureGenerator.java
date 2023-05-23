package io.proleap.cobol.analysis.issues.rules.procedure.condition;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.CobolBaseVisitor;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt.RelationConditionType;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;

@Singleton
public class IdenticalExpressionsLogicalRelationFeatureGenerator extends FeatureGenerator<RelationConditionValueStmt> {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Inject
	private CobolValueService valueService;

	@Inject
	private CobolValueStmtService valueStmtService;

	@Override
	public Stream<RelationConditionValueStmt> getAll(final CompilationUnit compilationUnit) {
		final Program program = compilationUnit.getProgram();
		final List<RelationConditionValueStmt> result = new ArrayList<RelationConditionValueStmt>();

		final CobolBaseVisitor<Boolean> visitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitRelationCondition(final CobolParser.RelationConditionContext ctx) {
				final RelationConditionValueStmt condition = (RelationConditionValueStmt) program
						.getASGElementRegistry().getASGElement(ctx);
				if (condition != null) {
					if (isRelevantRelation(condition)) {
						result.add(condition);
					}
				}

				return true;
			}
		};

		visitor.visit(compilationUnit.getCtx());
		return result.stream();
	}

	protected boolean isRelevantRelation(final RelationConditionValueStmt relation) {
		final boolean result;

		if (!RelationConditionType.ARITHMETIC.equals(relation.getRelationConditionType())) {
			result = false;
		} else {
			final ArithmeticValueStmt arithmeticExpressionLeft = relation.getArithmeticComparison()
					.getArithmeticExpressionLeft();
			final ArithmeticValueStmt arithmeticExpressionRight = relation.getArithmeticComparison()
					.getArithmeticExpressionRight();
			final DataDescriptionEntry leftDDE = cobolDataDescriptionEntryService
					.getDataDescriptionEntry(arithmeticExpressionLeft);
			final DataDescriptionEntry rightDDE = cobolDataDescriptionEntryService
					.getDataDescriptionEntry(arithmeticExpressionRight);

			if (rightDDE != null && leftDDE != null) {
				result = rightDDE.equals(leftDDE);
			} else {
				final CobolValue leftValue = valueStmtService.getValue(arithmeticExpressionLeft, null);
				final CobolValue rightValue = valueStmtService.getValue(arithmeticExpressionRight, null);

				if (leftValue != null && rightValue != null) {
					result = valueService.getValue(leftValue).equals(valueService.getValue(rightValue));
				} else {
					result = false;
				}
			}
		}

		return result;
	}
}
