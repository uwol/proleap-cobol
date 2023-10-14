package io.proleap.cobol.transform.java.rules.lang.valuestmt.arithmetic.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.MultDivsContext;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDivs;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class MultDivsRuleImpl extends CobolTransformRule<MultDivsContext, MultDivs> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final MultDivsContext ctx, final MultDivs multDivs, final RuleContext rc) {
		rc.p(javaExpressionService.mapToExpression(multDivs));
	}

	@Override
	public Class<MultDivsContext> from() {
		return MultDivsContext.class;
	}
}
