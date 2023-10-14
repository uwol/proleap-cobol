package io.proleap.cobol.transform.java.rules.lang.procedure.evaluate;

import java.util.List;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.EvaluateStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.EvaluateStatement;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.WhenOther;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.WhenPhrase;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class EvaluateStatementRule extends CobolTransformRule<EvaluateStatementContext, EvaluateStatement> {

	@Override
	public void apply(final EvaluateStatementContext ctx, final EvaluateStatement evaluateStatement,
			final RuleContext rc) {
		for (final WhenPhrase whenPhrase : evaluateStatement.getWhenPhrases()) {
			rc.p("{");
			rc.pNl();
			rc.getPrinter().indent();

			visitStatements(whenPhrase.getStatements(), rc);

			rc.getPrinter().unindent();
			rc.p("}");
			rc.pNl();
		}

		final WhenOther whenOther = evaluateStatement.getWhenOther();

		if (whenOther != null) {
			rc.p("{");
			rc.pNl();
			rc.getPrinter().indent();

			visitStatements(whenOther.getStatements(), rc);

			rc.getPrinter().unindent();
			rc.p("}");
			rc.pNl();
		}
	}

	@Override
	public Class<EvaluateStatementContext> from() {
		return EvaluateStatementContext.class;
	}

	protected void visitStatements(final List<Statement> statements, final RuleContext rc) {
		for (final Statement statement : statements) {
			rc.visit(statement.getCtx());
		}
	}
}
