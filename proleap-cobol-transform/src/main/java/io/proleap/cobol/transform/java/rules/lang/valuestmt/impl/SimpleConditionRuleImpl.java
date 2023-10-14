package io.proleap.cobol.transform.java.rules.lang.valuestmt.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.SimpleConditionContext;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.SimpleCondition;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class SimpleConditionRuleImpl extends CobolTransformRule<SimpleConditionContext, SimpleCondition> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final SimpleConditionContext ctx, final SimpleCondition simpleCondition, final RuleContext rc) {
		rc.p(javaExpressionService.mapToExpression(simpleCondition));
	}

	@Override
	public Class<SimpleConditionContext> from() {
		return SimpleConditionContext.class;
	}
}
