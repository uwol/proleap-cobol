package io.proleap.cobol.transform.java.rules.lang.valuestmt.arithmetic.impl;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.CobolParser.ArithmeticExpressionContext;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ArithmeticValueStmtRuleImpl extends CobolTransformRule<ArithmeticExpressionContext, ArithmeticValueStmt> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final ArithmeticExpressionContext ctx, final ArithmeticValueStmt arithmeticValueStmt,
			final RuleContext rc) {
		rc.p(javaExpressionService.mapToExpression(arithmeticValueStmt));
	}

	@Override
	public Class<ArithmeticExpressionContext> from() {
		return ArithmeticExpressionContext.class;
	}
}
