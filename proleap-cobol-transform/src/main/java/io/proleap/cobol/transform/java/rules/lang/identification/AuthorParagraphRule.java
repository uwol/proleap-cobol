package io.proleap.cobol.transform.java.rules.lang.identification;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.AuthorParagraphContext;
import io.proleap.cobol.asg.metamodel.identification.AuthorParagraph;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class AuthorParagraphRule extends CobolTransformRule<AuthorParagraphContext, AuthorParagraph> {

	@Override
	public void apply(final AuthorParagraphContext ctx, final AuthorParagraph authorParagraph, final RuleContext rc) {
		rc.p("* @author: %s", authorParagraph.getAuthor());
		rc.pNl(authorParagraph);
	}

	@Override
	public Class<AuthorParagraphContext> from() {
		return AuthorParagraphContext.class;
	}
}
