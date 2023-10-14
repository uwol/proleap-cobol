package io.proleap.cobol.transform.java.rules.lang;

import java.math.BigDecimal;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.NumericLiteralContext;
import io.proleap.cobol.asg.metamodel.NumericLiteral;
import io.proleap.cobol.transform.java.util.JavaLiteralUtils;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class NumericLiteralRule extends CobolTransformRule<NumericLiteralContext, NumericLiteral> {

	@Override
	public void apply(final NumericLiteralContext ctx, final NumericLiteral numericLiteral, final RuleContext rc) {
		final BigDecimal value = numericLiteral.getValue();
		rc.p(JavaLiteralUtils.mapToLiteral(value));
	}

	@Override
	public Class<NumericLiteralContext> from() {
		return NumericLiteralContext.class;
	}
}
