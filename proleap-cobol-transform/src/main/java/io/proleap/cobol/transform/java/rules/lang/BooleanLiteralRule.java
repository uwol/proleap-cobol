package io.proleap.cobol.transform.java.rules.lang;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.BooleanLiteralContext;
import io.proleap.cobol.asg.metamodel.BooleanLiteral;
import io.proleap.cobol.transform.java.util.JavaLiteralUtils;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class BooleanLiteralRule extends CobolTransformRule<BooleanLiteralContext, BooleanLiteral> {

	@Override
	public void apply(final BooleanLiteralContext ctx, final BooleanLiteral booleanLiteral, final RuleContext rc) {
		final Boolean value = booleanLiteral.getValue();
		rc.p(JavaLiteralUtils.mapToLiteral(value));
	}

	@Override
	public Class<BooleanLiteralContext> from() {
		return BooleanLiteralContext.class;
	}
}
