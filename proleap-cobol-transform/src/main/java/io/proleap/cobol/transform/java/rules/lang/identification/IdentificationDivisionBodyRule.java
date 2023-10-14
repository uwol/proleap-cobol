package io.proleap.cobol.transform.java.rules.lang.identification;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.IdentificationDivisionBodyContext;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class IdentificationDivisionBodyRule extends CobolTransformRule<IdentificationDivisionBodyContext, ASGElement> {

	@Override
	public void apply(final IdentificationDivisionBodyContext ctx, final ASGElement asgElement, final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<IdentificationDivisionBodyContext> from() {
		return IdentificationDivisionBodyContext.class;
	}
}
