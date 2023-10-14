package io.proleap.cobol.transform.java.rules.lang.valuestmt.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.AndOrConditionContext;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.AndOrCondition;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class AndOrConditionRuleImpl extends CobolTransformRule<AndOrConditionContext, AndOrCondition> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final AndOrConditionContext ctx, final AndOrCondition andOrCondition, final RuleContext rc) {
		rc.p(javaExpressionService.mapToExpression(andOrCondition));
	}

	@Override
	public Class<AndOrConditionContext> from() {
		return AndOrConditionContext.class;
	}
}
