package io.proleap.cobol.transform.java.rules.lang.procedure.set;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.SetStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.set.SetStatement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class SetStatementRule extends CobolTransformRule<SetStatementContext, SetStatement> {

	@Override
	public void apply(final SetStatementContext ctx, final SetStatement setStatement, final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<SetStatementContext> from() {
		return SetStatementContext.class;
	}
}
