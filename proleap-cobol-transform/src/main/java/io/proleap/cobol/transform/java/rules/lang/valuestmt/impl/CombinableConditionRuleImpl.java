package io.proleap.cobol.transform.java.rules.lang.valuestmt.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.CombinableConditionContext;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.CombinableCondition;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class CombinableConditionRuleImpl extends CobolTransformRule<CombinableConditionContext, CombinableCondition> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final CombinableConditionContext ctx, final CombinableCondition combinableCondition,
			final RuleContext rc) {
		rc.p(javaExpressionService.mapToExpression(combinableCondition));
	}

	@Override
	public Class<CombinableConditionContext> from() {
		return CombinableConditionContext.class;
	}
}
