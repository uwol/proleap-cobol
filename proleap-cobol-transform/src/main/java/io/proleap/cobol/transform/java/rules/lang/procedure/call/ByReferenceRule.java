package io.proleap.cobol.transform.java.rules.lang.procedure.call;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.CallByReferenceContext;
import io.proleap.cobol.asg.metamodel.procedure.call.ByReference;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ByReferenceRule extends CobolTransformRule<CallByReferenceContext, ByReference> {

	@Override
	public void apply(final CallByReferenceContext ctx, final ByReference byReference, final RuleContext rc) {

	}

	@Override
	public Class<CallByReferenceContext> from() {
		return CallByReferenceContext.class;
	}
}
