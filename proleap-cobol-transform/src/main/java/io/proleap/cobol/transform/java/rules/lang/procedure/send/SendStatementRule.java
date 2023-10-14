package io.proleap.cobol.transform.java.rules.lang.procedure.send;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.SendStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.NotOnExceptionClause;
import io.proleap.cobol.asg.metamodel.procedure.OnExceptionClause;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.send.SendStatement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class SendStatementRule extends CobolTransformRule<SendStatementContext, SendStatement> {

	@Override
	public void apply(final SendStatementContext ctx, final SendStatement sendStatement, final RuleContext rc) {
		final OnExceptionClause onExceptionClause = sendStatement.getOnExceptionClause();
		final NotOnExceptionClause notOnExceptionClause = sendStatement.getNotOnExceptionClause();

		rc.p("try {");
		rc.pNl();

		if (notOnExceptionClause != null) {
			visitStatements(notOnExceptionClause, rc);
		}

		rc.p("} catch (Exception e) {");
		rc.pNl();
		rc.getPrinter().indent();

		if (onExceptionClause != null) {
			visitStatements(onExceptionClause, rc);
		}

		rc.getPrinter().unindent();
		rc.p("}");
		rc.pNl(sendStatement);
	}

	@Override
	public Class<SendStatementContext> from() {
		return SendStatementContext.class;
	}

	private void visitStatements(final NotOnExceptionClause notOnExceptionClause, final RuleContext rc) {
		for (final Statement statement : notOnExceptionClause.getStatements()) {
			rc.visit(statement.getCtx());
		}
	}

	private void visitStatements(final OnExceptionClause onExceptionClause, final RuleContext rc) {
		for (final Statement statement : onExceptionClause.getStatements()) {
			rc.visit(statement.getCtx());
		}
	}
}
