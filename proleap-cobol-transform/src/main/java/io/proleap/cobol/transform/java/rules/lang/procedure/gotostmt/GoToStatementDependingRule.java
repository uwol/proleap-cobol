package io.proleap.cobol.transform.java.rules.lang.procedure.gotostmt;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.GoToDependingOnStatementContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.DependingOnPhrase;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class GoToStatementDependingRule
		extends AbstractGoToStatementRule<GoToDependingOnStatementContext, DependingOnPhrase> {

	@Override
	public void apply(final GoToDependingOnStatementContext ctx, final DependingOnPhrase dependingOn,
			final RuleContext rc) {
		final Call dependingOnCall = dependingOn.getDependingOnCall();

		int i = 1;

		for (final Call call : dependingOn.getProcedureCalls()) {
			rc.p("if(BigDecimal.valueOf(%s).equals(", i);

			if (dependingOnCall != null) {
				rc.visit(dependingOnCall.getCtx());
			}

			rc.p(")){");
			rc.pNl();
			rc.getPrinter().indent();

			printCall(call, true, rc);

			rc.getPrinter().unindent();
			rc.p("}");
			rc.pNl();

			i++;
		}
	}

	@Override
	public Class<GoToDependingOnStatementContext> from() {
		return GoToDependingOnStatementContext.class;
	}
}
