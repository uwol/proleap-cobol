package io.proleap.cobol.transform.java.rules.lang.procedure.set;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.SetUpDownByStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.set.SetBy;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class SetByRule extends CobolTransformRule<SetUpDownByStatementContext, SetBy> {

	@Override
	public void apply(final SetUpDownByStatementContext ctx, final SetBy setBy, final RuleContext rc) {
	}

	@Override
	public Class<SetUpDownByStatementContext> from() {
		return SetUpDownByStatementContext.class;
	}
}
