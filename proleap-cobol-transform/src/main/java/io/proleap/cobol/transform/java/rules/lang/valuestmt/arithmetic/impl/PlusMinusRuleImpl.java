package io.proleap.cobol.transform.java.rules.lang.valuestmt.arithmetic.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.PlusMinusContext;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.PlusMinus;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class PlusMinusRuleImpl extends CobolTransformRule<PlusMinusContext, PlusMinus> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final PlusMinusContext ctx, final PlusMinus plusMinus, final RuleContext rc) {
		rc.p(javaExpressionService.mapToExpression(plusMinus));
	}

	@Override
	public Class<PlusMinusContext> from() {
		return PlusMinusContext.class;
	}
}
