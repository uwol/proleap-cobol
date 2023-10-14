package io.proleap.cobol.transform.java.rules.lang.environment;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.EnvironmentDivisionBodyContext;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class EnvironmentDivisionBodyRule extends CobolTransformRule<EnvironmentDivisionBodyContext, ASGElement> {

	@Override
	public void apply(final EnvironmentDivisionBodyContext ctx, final ASGElement asgElement, final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<EnvironmentDivisionBodyContext> from() {
		return EnvironmentDivisionBodyContext.class;
	}
}
