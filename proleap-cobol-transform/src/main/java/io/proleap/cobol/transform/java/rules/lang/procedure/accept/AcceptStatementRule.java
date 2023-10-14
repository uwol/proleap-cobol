package io.proleap.cobol.transform.java.rules.lang.procedure.accept;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.AcceptStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.NotOnExceptionClause;
import io.proleap.cobol.asg.metamodel.procedure.OnExceptionClause;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.accept.AcceptStatement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class AcceptStatementRule extends CobolTransformRule<AcceptStatementContext, AcceptStatement> {

	@Override
	public void apply(final AcceptStatementContext ctx, final AcceptStatement acceptStatement, final RuleContext rc) {
		final OnExceptionClause onExceptionClause = acceptStatement.getOnExceptionClause();
		final NotOnExceptionClause notOnExceptionClause = acceptStatement.getNotOnExceptionClause();

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
		rc.pNl(acceptStatement);
	}

	@Override
	public Class<AcceptStatementContext> from() {
		return AcceptStatementContext.class;
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
