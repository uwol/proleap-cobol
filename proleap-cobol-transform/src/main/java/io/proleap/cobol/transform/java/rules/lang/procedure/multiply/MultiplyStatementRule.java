package io.proleap.cobol.transform.java.rules.lang.procedure.multiply;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.MultiplyStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.multiply.ByOperand;
import io.proleap.cobol.asg.metamodel.procedure.multiply.ByPhrase;
import io.proleap.cobol.asg.metamodel.procedure.multiply.GivingPhrase;
import io.proleap.cobol.asg.metamodel.procedure.multiply.GivingResult;
import io.proleap.cobol.asg.metamodel.procedure.multiply.MultiplyStatement;
import io.proleap.cobol.asg.metamodel.procedure.multiply.MultiplyStatement.MultiplyType;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class MultiplyStatementRule extends CobolTransformRule<MultiplyStatementContext, MultiplyStatement> {

	@Override
	public void apply(final MultiplyStatementContext ctx, final MultiplyStatement multiplyStatement,
			final RuleContext rc) {
		final MultiplyType type = multiplyStatement.getMultiplyType();

		switch (type) {
		case BY_GIVING:
			printMultiplyGiving(multiplyStatement, rc);
			break;
		case BY:
			printMultiplyRegular(multiplyStatement, rc);
			break;
		default:
			break;
		}
	}

	@Override
	public Class<MultiplyStatementContext> from() {
		return MultiplyStatementContext.class;
	}

	protected void printGiving(final MultiplyStatement multiplyStatement, final GivingPhrase giving,
			final GivingResult givingResult, final RuleContext rc) {
		rc.visit(givingResult.getResultCall().getCtx());
		rc.p(" = ");
		rc.visit(multiplyStatement.getOperandValueStmt().getCtx());
		rc.p(".multiply(");
		rc.visit(giving.getGivingOperand().getOperandValueStmt().getCtx());
		rc.p(");");

		rc.pNl(givingResult);
	}

	protected void printMultiplyGiving(final MultiplyStatement multiplyStatement, final RuleContext rc) {
		final GivingPhrase giving = multiplyStatement.getGivingPhrase();

		for (final GivingResult givingResult : giving.getGivingResults()) {
			printGiving(multiplyStatement, giving, givingResult, rc);
		}
	}

	protected void printMultiplyRegular(final MultiplyStatement multiplyStatement, final RuleContext rc) {
		final ByPhrase regular = multiplyStatement.getByPhrase();

		rc.visit(multiplyStatement.getOperandValueStmt().getCtx());
		rc.p(" = ");
		rc.visit(multiplyStatement.getOperandValueStmt().getCtx());

		for (final ByOperand regularOperand : regular.getByOperands()) {
			rc.p(".multiply(");
			rc.visit(regularOperand.getOperandCall().getCtx());
			rc.p(")");
		}

		rc.p(";");
		rc.pNl(regular);
	}
}
