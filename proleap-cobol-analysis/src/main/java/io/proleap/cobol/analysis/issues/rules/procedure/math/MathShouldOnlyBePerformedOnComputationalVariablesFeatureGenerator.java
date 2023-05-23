package io.proleap.cobol.analysis.issues.rules.procedure.math;

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
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.UsageClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.UsageClause.UsageClauseType;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Basis;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;

@Singleton
public class MathShouldOnlyBePerformedOnComputationalVariablesFeatureGenerator
		extends FeatureGenerator<ArithmeticValueStmt> {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Override
	public Stream<ArithmeticValueStmt> getAll(final CompilationUnit compilationUnit) {
		final Program program = compilationUnit.getProgram();
		final List<ArithmeticValueStmt> result = new ArrayList<ArithmeticValueStmt>();

		final CobolBaseVisitor<Boolean> visitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitArithmeticExpression(final CobolParser.ArithmeticExpressionContext ctx) {
				final ArithmeticValueStmt arithmeticValueStmt = (ArithmeticValueStmt) program.getASGElementRegistry()
						.getASGElement(ctx);
				if (arithmeticValueStmt != null) {
					if (isRelevantArithmeticValueStmt(arithmeticValueStmt)) {
						result.add(arithmeticValueStmt);
					}
				}
				return true;
			}
		};

		visitor.visit(compilationUnit.getCtx());
		return result.stream();
	}

	protected boolean isRelevantArithmeticValueStmt(final ArithmeticValueStmt arithmeticValueStmt) {
		final List<Call> relevantCallList = new ArrayList<Call>();
		final List<Basis> relevantBasisList = new ArrayList<Basis>();
		final Program program = arithmeticValueStmt.getCompilationUnit().getProgram();

		final boolean result;

		final CobolBaseVisitor<Boolean> identifierVisitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitIdentifier(final CobolParser.IdentifierContext ctx) {
				final Call call = (Call) program.getASGElementRegistry().getASGElement(ctx);

				if (isRelevantCall(call)) {
					relevantCallList.add(call);
				}

				return true;
			}
		};

		final CobolBaseVisitor<Boolean> basisVisitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitBasis(final CobolParser.BasisContext ctx) {
				final Basis basis = (Basis) program.getASGElementRegistry().getASGElement(ctx);

				relevantBasisList.add(basis);

				return true;
			}
		};

		basisVisitor.visit(arithmeticValueStmt.getCtx());

		if (relevantBasisList.size() < 2) {
			result = false;
		} else {
			identifierVisitor.visit(arithmeticValueStmt.getCtx());

			if (relevantCallList.size() == 0) {
				result = false;
			} else {
				result = true;
			}
		}

		return result;
	}

	protected boolean isRelevantCall(final Call call) {
		final DataDescriptionEntry dataDescriptionEntry = cobolDataDescriptionEntryService
				.getDataDescriptionEntry(call);
		final boolean result;

		if (dataDescriptionEntry == null) {
			result = false;
		} else {
			final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry
					.getDataDescriptionEntryType();

			if (!DataDescriptionEntry.DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)) {
				result = false;
			} else {
				final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
				final UsageClause usageClause = dataDescriptionEntryGroup.getUsageClause();

				if (usageClause == null) {
					result = true;
				} else {
					final UsageClauseType usageClauseType = usageClause.getUsageClauseType();

					result = isRelevantClauseType(usageClauseType);
				}
			}
		}

		return result;
	}

	protected boolean isRelevantClauseType(final UsageClauseType usageClauseType) {
		final boolean result;

		switch (usageClauseType) {
		case BINARY:
			result = false;
			break;
		case COMP:
			result = false;
			break;
		case COMP_1:
			result = false;
			break;
		case COMP_2:
			result = false;
			break;
		case COMP_3:
			result = false;
			break;
		case COMP_4:
			result = false;
			break;
		case COMP_5:
			result = false;
			break;
		default:
			result = true;
			break;
		}

		return result;
	}
}
