package io.proleap.cobol.transform.java.rules.lang.identification;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.DateWrittenParagraphContext;
import io.proleap.cobol.asg.metamodel.identification.DateWrittenParagraph;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class DateWrittenParagraphRule extends CobolTransformRule<DateWrittenParagraphContext, DateWrittenParagraph> {

	@Override
	public void apply(final DateWrittenParagraphContext ctx, final DateWrittenParagraph dateWrittenParagraph,
			final RuleContext rc) {
		rc.p("* @created: %s", dateWrittenParagraph.getDateWritten());
		rc.pNl(dateWrittenParagraph);
	}

	@Override
	public Class<DateWrittenParagraphContext> from() {
		return DateWrittenParagraphContext.class;
	}
}
