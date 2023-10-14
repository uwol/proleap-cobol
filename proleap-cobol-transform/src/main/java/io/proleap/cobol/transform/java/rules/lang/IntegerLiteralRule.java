package io.proleap.cobol.transform.java.rules.lang;

import java.math.BigDecimal;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.IntegerLiteralContext;
import io.proleap.cobol.asg.metamodel.IntegerLiteral;
import io.proleap.cobol.transform.java.util.JavaLiteralUtils;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class IntegerLiteralRule extends CobolTransformRule<IntegerLiteralContext, IntegerLiteral> {

	@Override
	public void apply(final IntegerLiteralContext ctx, final IntegerLiteral integerLiteral, final RuleContext rc) {
		final BigDecimal value = integerLiteral.getValue();
		rc.p(JavaLiteralUtils.mapToLiteral(value));
	}

	@Override
	public Class<IntegerLiteralContext> from() {
		return IntegerLiteralContext.class;
	}
}
