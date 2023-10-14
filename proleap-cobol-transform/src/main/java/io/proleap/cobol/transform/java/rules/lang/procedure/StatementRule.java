package io.proleap.cobol.transform.java.rules.lang.procedure;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.StatementContext;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class StatementRule extends CobolTransformRule<StatementContext, ASGElement> {

	@Override
	public void apply(final StatementContext ctx, final ASGElement semanticGraphElement, final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<StatementContext> from() {
		return StatementContext.class;
	}
}
