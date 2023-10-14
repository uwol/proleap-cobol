package io.proleap.cobol.transform.java.rules.lang.valuestmt.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.RelationArithmeticComparisonContext;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.ArithmeticComparison;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class RelationArithmeticComparisonRuleImpl
		extends CobolTransformRule<RelationArithmeticComparisonContext, ArithmeticComparison> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final RelationArithmeticComparisonContext ctx, final ArithmeticComparison arithmeticComparison,
			final RuleContext rc) {
		rc.p(javaExpressionService.mapToExpression(arithmeticComparison));
	}

	@Override
	public Class<RelationArithmeticComparisonContext> from() {
		return RelationArithmeticComparisonContext.class;
	}
}
