package io.proleap.cobol.transform.java.rules.lang.procedure.receive;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ReceiveStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.NotOnExceptionClause;
import io.proleap.cobol.asg.metamodel.procedure.OnExceptionClause;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.receive.ReceiveStatement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ReceiveStatementRule extends CobolTransformRule<ReceiveStatementContext, ReceiveStatement> {

	@Override
	public void apply(final ReceiveStatementContext ctx, final ReceiveStatement receiveStatement,
			final RuleContext rc) {
		final OnExceptionClause onExceptionClause = receiveStatement.getOnExceptionClause();
		final NotOnExceptionClause notOnExceptionClause = receiveStatement.getNotOnExceptionClause();

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
		rc.pNl(receiveStatement);
	}

	@Override
	public Class<ReceiveStatementContext> from() {
		return ReceiveStatementContext.class;
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
