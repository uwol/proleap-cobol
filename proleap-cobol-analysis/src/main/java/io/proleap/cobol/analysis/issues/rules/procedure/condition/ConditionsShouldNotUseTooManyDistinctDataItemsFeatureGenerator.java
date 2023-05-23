package io.proleap.cobol.analysis.issues.rules.procedure.condition;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
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
import io.proleap.cobol.asg.metamodel.valuestmt.ConditionValueStmt;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;

@Singleton
public class ConditionsShouldNotUseTooManyDistinctDataItemsFeatureGenerator
		extends FeatureGenerator<ConditionValueStmt> {

	final static int MAX_DATA_ITEMS = 3;

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Override
	public Stream<ConditionValueStmt> getAll(final CompilationUnit compilationUnit) {
		final Program program = compilationUnit.getProgram();
		final List<ConditionValueStmt> result = new ArrayList<ConditionValueStmt>();

		final CobolBaseVisitor<Boolean> visitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitCondition(final CobolParser.ConditionContext ctx) {
				final ConditionValueStmt condition = (ConditionValueStmt) program.getASGElementRegistry()
						.getASGElement(ctx);

				if (isRelevantCondition(condition, program)) {
					result.add(condition);
				}

				return true;
			}
		};

		visitor.visit(compilationUnit.getCtx());
		return result.stream();
	}

	protected boolean isRelevantCondition(final ConditionValueStmt condition, final Program program) {
		final Set<DataDescriptionEntry> dataDescriptionEntrySet = new HashSet<DataDescriptionEntry>();

		final CobolBaseVisitor<Boolean> visitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitArithmeticExpression(final CobolParser.ArithmeticExpressionContext ctx) {
				final ArithmeticValueStmt call = (ArithmeticValueStmt) program.getASGElementRegistry()
						.getASGElement(ctx);
				final DataDescriptionEntry dataDescriptionEntry = cobolDataDescriptionEntryService
						.getDataDescriptionEntry(call);

				if (dataDescriptionEntry != null) {
					dataDescriptionEntrySet.add(dataDescriptionEntry);
				}

				return true;
			}
		};

		visitor.visit(condition.getCtx());
		return (dataDescriptionEntrySet.size() > MAX_DATA_ITEMS);
	}
}
