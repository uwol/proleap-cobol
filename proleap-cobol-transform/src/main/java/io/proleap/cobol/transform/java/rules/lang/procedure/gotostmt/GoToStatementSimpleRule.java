package io.proleap.cobol.transform.java.rules.lang.procedure.gotostmt;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.GoToStatementSimpleContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.Simple;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class GoToStatementSimpleRule extends AbstractGoToStatementRule<GoToStatementSimpleContext, Simple> {

	@Override
	public void apply(final GoToStatementSimpleContext ctx, final Simple simple, final RuleContext rc) {
		final Call call = simple.getProcedureCall();
		printCall(call, true, rc);
	}

	@Override
	public Class<GoToStatementSimpleContext> from() {
		return GoToStatementSimpleContext.class;
	}
}
