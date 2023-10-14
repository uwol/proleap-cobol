package io.proleap.cobol.transform.java.rules.lang.identification;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.SecurityParagraphContext;
import io.proleap.cobol.asg.metamodel.identification.SecurityParagraph;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class SecurityParagraphRule extends CobolTransformRule<SecurityParagraphContext, SecurityParagraph> {

	@Override
	public void apply(final SecurityParagraphContext ctx, final SecurityParagraph securityParagraph,
			final RuleContext rc) {
		rc.p("* Security: %s", securityParagraph.getSecurity());
		rc.pNl(securityParagraph);
	}

	@Override
	public Class<SecurityParagraphContext> from() {
		return SecurityParagraphContext.class;
	}
}
