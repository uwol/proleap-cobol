package io.proleap.cobol.transform.java.rules.lang.procedure;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ParagraphsContext;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ParagraphsRule extends CobolTransformRule<ParagraphsContext, ASGElement> {

	@Override
	public void apply(final ParagraphsContext ctx, final ASGElement semanticGraphElement, final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<ParagraphsContext> from() {
		return ParagraphsContext.class;
	}
}
