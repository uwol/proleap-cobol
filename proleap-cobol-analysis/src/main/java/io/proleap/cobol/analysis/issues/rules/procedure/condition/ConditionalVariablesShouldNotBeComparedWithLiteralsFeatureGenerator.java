package io.proleap.cobol.analysis.issues.rules.procedure.condition;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolBaseVisitor;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.ArithmeticComparison;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;

@Singleton
public class ConditionalVariablesShouldNotBeComparedWithLiteralsFeatureGenerator
		extends FeatureGenerator<RelationConditionValueStmt> {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Override
	public Stream<RelationConditionValueStmt> getAll(final CompilationUnit compilationUnit) {
		final Program program = compilationUnit.getProgram();
		final List<RelationConditionValueStmt> result = new ArrayList<RelationConditionValueStmt>();

		final CobolBaseVisitor<Boolean> visitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitRelationCondition(final CobolParser.RelationConditionContext ctx) {
				final RelationConditionValueStmt relationConditionValueStmt = (RelationConditionValueStmt) program
						.getASGElementRegistry().getASGElement(ctx);

				if (isRelevantRelation(relationConditionValueStmt)) {
					result.add(relationConditionValueStmt);
				}

				return true;
			}
		};

		visitor.visit(compilationUnit.getCtx());
		return result.stream();
	}

	protected boolean isConditionalVariable(final DataDescriptionEntry dataDescriptionEntry) {
		final boolean result;
		final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry.getDataDescriptionEntryType();

		if (!DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)) {
			result = false;
		} else {
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
			final List<DataDescriptionEntry> dataDescriptionEntries = dataDescriptionEntryGroup
					.getDataDescriptionEntries();

			dataDescriptionEntries.removeIf(entry -> !DataDescriptionEntry.DataDescriptionEntryType.CONDITION
					.equals(entry.getDataDescriptionEntryType()));

			result = !dataDescriptionEntries.isEmpty();
		}

		return result;
	}

	protected boolean isRelevantRelation(final RelationConditionValueStmt condition) {
		final ArithmeticComparison arithmeticComparison = condition.getArithmeticComparison();
		final boolean result;

		if (arithmeticComparison == null) {
			result = false;
		} else {
			final ArithmeticValueStmt arithmeticExpressionLeft = arithmeticComparison.getArithmeticExpressionLeft();
			final ArithmeticValueStmt arithmeticExpressionRight = arithmeticComparison.getArithmeticExpressionRight();

			if (arithmeticExpressionLeft == null) {
				result = false;
			} else if (arithmeticExpressionRight == null) {
				result = false;
			} else {
				final DataDescriptionEntry dataDescriptionEntryRight = cobolDataDescriptionEntryService
						.getDataDescriptionEntry(arithmeticExpressionRight);
				final DataDescriptionEntry dataDescriptionEntryLeft = cobolDataDescriptionEntryService
						.getDataDescriptionEntry(arithmeticExpressionLeft);

				if (dataDescriptionEntryRight != null && dataDescriptionEntryLeft != null) {
					result = false;
				} else if (dataDescriptionEntryRight == null && dataDescriptionEntryLeft == null) {
					result = false;
				} else if (dataDescriptionEntryLeft != null) {
					result = isConditionalVariable(dataDescriptionEntryLeft);
				} else {
					result = isConditionalVariable(dataDescriptionEntryRight);
				}
			}
		}

		return result;
	}
}
