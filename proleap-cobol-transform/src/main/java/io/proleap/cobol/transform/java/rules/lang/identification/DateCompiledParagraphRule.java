package io.proleap.cobol.transform.java.rules.lang.identification;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.DateCompiledParagraphContext;
import io.proleap.cobol.asg.metamodel.identification.DateCompiledParagraph;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class DateCompiledParagraphRule extends CobolTransformRule<DateCompiledParagraphContext, DateCompiledParagraph> {

	@Override
	public void apply(final DateCompiledParagraphContext ctx, final DateCompiledParagraph dateCompiledParagraph,
			final RuleContext rc) {
		rc.p("* @compiled: %s", dateCompiledParagraph.getDateCompiled());
		rc.pNl(dateCompiledParagraph);
	}

	@Override
	public Class<DateCompiledParagraphContext> from() {
		return DateCompiledParagraphContext.class;
	}
}
