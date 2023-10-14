package io.proleap.cobol.transform.java.rules.lang.procedure.divide;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.DivideStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.divide.DivideByGivingStatement;
import io.proleap.cobol.asg.metamodel.procedure.divide.DivideIntoGivingStatement;
import io.proleap.cobol.asg.metamodel.procedure.divide.DivideIntoStatement;
import io.proleap.cobol.asg.metamodel.procedure.divide.DivideStatement;
import io.proleap.cobol.asg.metamodel.procedure.divide.DivideStatement.DivideType;
import io.proleap.cobol.asg.metamodel.procedure.divide.Giving;
import io.proleap.cobol.asg.metamodel.procedure.divide.GivingPhrase;
import io.proleap.cobol.asg.metamodel.procedure.divide.Into;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class DivideStatementRule extends CobolTransformRule<DivideStatementContext, DivideStatement> {

	@Override
	public void apply(final DivideStatementContext ctx, final DivideStatement divideStatement, final RuleContext rc) {
		final DivideType type = divideStatement.getDivideType();

		switch (type) {
		case BY_GIVING:
			printByGiving(divideStatement, rc);
			break;
		case INTO:
			printInto(divideStatement, rc);
			break;
		case INTO_GIVING:
			printIntoGiving(divideStatement, rc);
			break;
		default:
			break;
		}
	}

	@Override
	public Class<DivideStatementContext> from() {
		return DivideStatementContext.class;
	}

	protected void printByGiving(final DivideStatement divideStatement, final RuleContext rc) {
		final DivideByGivingStatement byGiving = divideStatement.getDivideByGivingStatement();
		final GivingPhrase givings = byGiving.getGivingPhrase();

		if (givings == null) {
			rc.visit(divideStatement.getOperandValueStmt().getCtx());
			rc.p(" = ");
			rc.visit(divideStatement.getOperandValueStmt().getCtx());
			rc.p(".divide(");
			rc.visit(byGiving.getByValueStmt().getCtx());
			rc.p(");");

			rc.pNl(divideStatement);
		} else {
			for (final Giving giving : givings.getGivings()) {
				rc.visit(giving.getGivingCall().getCtx());
				rc.p(" = ");
				rc.visit(divideStatement.getOperandValueStmt().getCtx());
				rc.p(".divide(");
				rc.visit(byGiving.getByValueStmt().getCtx());
				rc.p(");");

				rc.pNl(giving);
			}
		}
	}

	protected void printInto(final DivideStatement divideStatement, final RuleContext rc) {
		final DivideIntoStatement intoStatement = divideStatement.getDivideIntoStatement();

		for (final Into into : intoStatement.getIntos()) {
			rc.visit(into.getGivingCall().getCtx());
			rc.p(" = ");
			rc.visit(into.getGivingCall().getCtx());
			rc.p(".divide(");
			rc.visit(divideStatement.getOperandValueStmt().getCtx());
			rc.p(");");

			rc.pNl(into);
		}
	}

	protected void printIntoGiving(final DivideStatement divideStatement, final RuleContext rc) {
		final DivideIntoGivingStatement intoGivingStatement = divideStatement.getDivideIntoGivingStatement();
		final GivingPhrase givings = intoGivingStatement.getGivingPhrase();

		if (givings == null) {
			rc.visit(intoGivingStatement.getIntoValueStmt().getCtx());
			rc.p(" = ");
			rc.visit(intoGivingStatement.getIntoValueStmt().getCtx());
			rc.p(".divide(");
			rc.visit(divideStatement.getOperandValueStmt().getCtx());
			rc.p(");");

			rc.pNl(divideStatement);
		} else {
			for (final Giving giving : givings.getGivings()) {
				rc.visit(giving.getGivingCall().getCtx());
				rc.p(" = ");
				rc.visit(intoGivingStatement.getIntoValueStmt().getCtx());
				rc.p(".divide(");
				rc.visit(divideStatement.getOperandValueStmt().getCtx());
				rc.p(");");

				rc.pNl(giving);
			}
		}
	}
}
