package io.proleap.cobol.transform.java.rules.lang.valuestmt.arithmetic.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.MultDivContext;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDiv;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class MultDivRuleImpl extends CobolTransformRule<MultDivContext, MultDiv> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final MultDivContext ctx, final MultDiv multDiv, final RuleContext rc) {
		rc.p(javaExpressionService.mapToExpression(multDiv));
	}

	@Override
	public Class<MultDivContext> from() {
		return MultDivContext.class;
	}
}
