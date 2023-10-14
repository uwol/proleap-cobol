package io.proleap.cobol.transform.java.rules.lang.procedure.perform;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.PerformStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class PerformStatementRule extends CobolTransformRule<PerformStatementContext, PerformStatement> {

	@Override
	public void apply(final PerformStatementContext ctx, final PerformStatement performStatement,
			final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<PerformStatementContext> from() {
		return PerformStatementContext.class;
	}
}
