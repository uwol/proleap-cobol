package io.proleap.cobol.transform.java.rules.lang.valuestmt.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ConditionContext;
import io.proleap.cobol.asg.metamodel.valuestmt.ConditionValueStmt;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ConditionValueStmtRuleImpl extends CobolTransformRule<ConditionContext, ConditionValueStmt> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final ConditionContext ctx, final ConditionValueStmt conditionValueStmt, final RuleContext rc) {
		rc.p(javaExpressionService.mapToExpression(conditionValueStmt));
	}

	@Override
	public Class<ConditionContext> from() {
		return ConditionContext.class;
	}
}
