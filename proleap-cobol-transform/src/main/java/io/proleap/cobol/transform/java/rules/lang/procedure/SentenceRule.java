package io.proleap.cobol.transform.java.rules.lang.procedure;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.SentenceContext;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class SentenceRule extends CobolTransformRule<SentenceContext, ASGElement> {

	@Override
	public void apply(final SentenceContext ctx, final ASGElement semanticGraphElement, final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<SentenceContext> from() {
		return SentenceContext.class;
	}
}
