package io.proleap.cobol.transform.java.rules.lang.procedure.call;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.CallStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.NotOnExceptionClause;
import io.proleap.cobol.asg.metamodel.procedure.OnExceptionClause;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.call.CallStatement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class CallStatementRule extends CobolTransformRule<CallStatementContext, CallStatement> {

	@Override
	public void apply(final CallStatementContext ctx, final CallStatement callStatement, final RuleContext rc) {
		final OnExceptionClause onExceptionClause = callStatement.getOnExceptionClause();
		final NotOnExceptionClause notOnExceptionClause = callStatement.getNotOnExceptionClause();

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
		rc.pNl(callStatement);
	}

	@Override
	public Class<CallStatementContext> from() {
		return CallStatementContext.class;
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
