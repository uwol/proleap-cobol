package io.proleap.cobol.transform.java.rules.lang.procedure.continuestmt;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ContinueStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.continuestmt.ContinueStatement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ContinueRule extends CobolTransformRule<ContinueStatementContext, ContinueStatement> {

	@Override
	public void apply(final ContinueStatementContext ctx, final ContinueStatement continueStatement,
			final RuleContext rc) {
	}

	@Override
	public Class<ContinueStatementContext> from() {
		return ContinueStatementContext.class;
	}
}
