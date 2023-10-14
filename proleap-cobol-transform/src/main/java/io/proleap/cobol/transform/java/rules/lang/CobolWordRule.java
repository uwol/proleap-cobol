package io.proleap.cobol.transform.java.rules.lang;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.CobolWordContext;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class CobolWordRule extends CobolTransformRule<CobolWordContext, ASGElement> {

	@Override
	public void apply(final CobolWordContext ctx, final ASGElement asgElement, final RuleContext rc) {
		rc.p("\"%s\"", ctx.getText());
	}

	@Override
	public Class<CobolWordContext> from() {
		return CobolWordContext.class;
	}
}
