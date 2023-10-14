package io.proleap.cobol.transform.java.rules.lang.valuestmt.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.RelationConditionContext;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class RelationConditionRuleImpl extends CobolTransformRule<RelationConditionContext, RelationConditionValueStmt> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final RelationConditionContext ctx, final RelationConditionValueStmt relationCondition,
			final RuleContext rc) {
		rc.p(javaExpressionService.mapToExpression(relationCondition));
	}

	@Override
	public Class<RelationConditionContext> from() {
		return RelationConditionContext.class;
	}
}
