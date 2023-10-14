package io.proleap.cobol.transform.java.rules.lang.environment;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.EnvironmentDivisionContext;
import io.proleap.cobol.asg.metamodel.environment.EnvironmentDivision;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class EnvironmentDivisionRule extends CobolTransformRule<EnvironmentDivisionContext, EnvironmentDivision> {

	@Override
	public void apply(final EnvironmentDivisionContext ctx, final EnvironmentDivision environmentDivision,
			final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<EnvironmentDivisionContext> from() {
		return EnvironmentDivisionContext.class;
	}
}
