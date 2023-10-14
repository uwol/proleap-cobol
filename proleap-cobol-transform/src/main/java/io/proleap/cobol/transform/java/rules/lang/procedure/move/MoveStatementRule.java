package io.proleap.cobol.transform.java.rules.lang.procedure.move;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.MoveStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveStatement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class MoveStatementRule extends CobolTransformRule<MoveStatementContext, MoveStatement> {

	@Override
	public void apply(final MoveStatementContext ctx, final MoveStatement moveStatement, final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<MoveStatementContext> from() {
		return MoveStatementContext.class;
	}
}
