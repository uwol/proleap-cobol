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
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDiv;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDiv.MultDivType;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDivs;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.PlusMinus;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.PlusMinus.PlusMinusType;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Powers;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;

@Singleton
public class IdenticalExpressionsArithmeticOperatorFeatureGenerator extends FeatureGenerator<ArithmeticValueStmt> {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Inject
	private CobolValueService valueService;

	@Inject
	private CobolValueStmtService valueStmtService;

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

	protected boolean hasSameValue(final CobolValue leftValue, final CobolValue rightValue) {
		final boolean result;

		if (leftValue != null && rightValue != null) {
			result = valueService.getValue(leftValue).equals(valueService.getValue(rightValue));
		} else {
			result = false;
		}

		return result;
	}

	protected boolean isRelevantArithmeticValueStmt(final ArithmeticValueStmt arithmeticValueStmt) {
		final MultDivs multDivs = arithmeticValueStmt.getMultDivs();
		final boolean result;

		if (arithmeticValueStmt.getPlusMinus().isEmpty()) {
			result = isRelevantMultDivs(multDivs);
		} else if (arithmeticValueStmt.getPlusMinus().size() == 1) {
			result = isRelevantPlusMinus(arithmeticValueStmt);
		} else {
			result = false;
		}

		return result;
	}

	protected boolean isRelevantMultDivs(final MultDivs multDivs) {
		final Powers powersLeft = multDivs.getPowers();
		final List<MultDiv> multDivList = multDivs.getMultDivs();

		final boolean result;

		if (multDivList.size() != 1) {
			result = false;
		} else {
			final MultDiv multDiv = multDivList.get(0);

			if (!MultDivType.DIV.equals(multDiv.getMultDivType())) {
				result = false;
			} else {
				final Powers powersRight = multDiv.getPowers();

				final DataDescriptionEntry dataDescriptionEntryLeft = cobolDataDescriptionEntryService
						.getDataDescriptionEntry(powersLeft);
				final DataDescriptionEntry dataDescriptionEntryRight = cobolDataDescriptionEntryService
						.getDataDescriptionEntry(powersRight);

				if (dataDescriptionEntryLeft != null && dataDescriptionEntryRight != null) {
					result = dataDescriptionEntryLeft.equals(dataDescriptionEntryRight);
				} else if (dataDescriptionEntryLeft == null && dataDescriptionEntryRight == null) {
					final CobolValue leftValue = valueStmtService.getValue(powersLeft, null);
					final CobolValue rightValue = valueStmtService.getValue(powersRight, null);
					result = hasSameValue(leftValue, rightValue);
				} else {
					result = false;
				}
			}
		}

		return result;
	}

	protected boolean isRelevantPlusMinus(final ArithmeticValueStmt arithmeticValueStmt) {
		final MultDivs multDivsLeft = arithmeticValueStmt.getMultDivs();
		final boolean result;

		if (arithmeticValueStmt.getPlusMinus().size() != 1) {
			result = false;
		} else if (!multDivsLeft.getMultDivs().isEmpty()) {
			result = false;
		} else {
			final PlusMinus plusMinusRight = arithmeticValueStmt.getPlusMinus().get(0);

			if (!PlusMinusType.MINUS.equals(plusMinusRight.getPlusMinusType())) {
				result = false;
			} else {
				final DataDescriptionEntry dataDescriptionEntryLeft = cobolDataDescriptionEntryService
						.getDataDescriptionEntry(multDivsLeft);
				final DataDescriptionEntry dataDescriptionEntryRight = cobolDataDescriptionEntryService
						.getDataDescriptionEntry(plusMinusRight);

				if (dataDescriptionEntryLeft != null && dataDescriptionEntryRight != null) {
					result = dataDescriptionEntryLeft.equals(dataDescriptionEntryRight);
				} else if (dataDescriptionEntryLeft == null && dataDescriptionEntryRight == null) {
					final CobolValue leftValue = valueStmtService.getValue(multDivsLeft, null);
					final MultDivs multDivsRight = plusMinusRight.getMultDivs();
					final CobolValue rightValue = valueStmtService.getValue(multDivsRight, null);

					result = hasSameValue(leftValue, rightValue);
				} else {
					result = false;
				}
			}
		}

		return result;
	}
}
