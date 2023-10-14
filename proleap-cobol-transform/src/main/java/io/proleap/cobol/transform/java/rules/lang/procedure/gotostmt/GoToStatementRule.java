package io.proleap.cobol.transform.java.rules.lang.procedure.gotostmt;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.GoToStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.GoToStatement;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.GoToStatement.GoToType;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class GoToStatementRule extends CobolTransformRule<GoToStatementContext, GoToStatement> {

	@Override
	public void apply(final GoToStatementContext ctx, final GoToStatement goToStatement, final RuleContext rc) {
		final GoToType type = goToStatement.getGoToType();

		switch (type) {
		case DEPENDING_ON:
			rc.visit(goToStatement.getDependingOnPhrase().getCtx());
			break;
		case SIMPLE:
			rc.visit(goToStatement.getSimple().getCtx());
			break;
		default:
			break;
		}
	}

	@Override
	public Class<GoToStatementContext> from() {
		return GoToStatementContext.class;
	}
}
