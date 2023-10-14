package io.proleap.cobol.transform.java.rules.lang.valuestmt.arithmetic.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.PowersContext;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Powers;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class PowersRuleImpl extends CobolTransformRule<PowersContext, Powers> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final PowersContext ctx, final Powers powers, final RuleContext rc) {
		rc.p(javaExpressionService.mapToExpression(powers));
	}

	@Override
	public Class<PowersContext> from() {
		return PowersContext.class;
	}
}
