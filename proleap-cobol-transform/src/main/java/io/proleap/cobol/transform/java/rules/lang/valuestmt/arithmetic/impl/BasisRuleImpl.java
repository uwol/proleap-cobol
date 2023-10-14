package io.proleap.cobol.transform.java.rules.lang.valuestmt.arithmetic.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.BasisContext;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Basis;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class BasisRuleImpl extends CobolTransformRule<BasisContext, Basis> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final BasisContext ctx, final Basis basis, final RuleContext rc) {
		rc.p(javaExpressionService.mapToExpression(basis));
	}

	@Override
	public Class<BasisContext> from() {
		return BasisContext.class;
	}
}
