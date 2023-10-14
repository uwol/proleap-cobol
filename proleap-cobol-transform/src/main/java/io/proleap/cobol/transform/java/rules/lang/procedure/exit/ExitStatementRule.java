package io.proleap.cobol.transform.java.rules.lang.procedure.exit;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ExitStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.exit.ExitStatement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ExitStatementRule extends CobolTransformRule<ExitStatementContext, ExitStatement> {

	@Override
	public void apply(final ExitStatementContext ctx, final ExitStatement divideStatement, final RuleContext rc) {
		rc.p("System.exit(0);");
		rc.pNl(divideStatement);
	}

	@Override
	public Class<ExitStatementContext> from() {
		return ExitStatementContext.class;
	}
}
