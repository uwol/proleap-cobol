package io.proleap.cobol.transform.java.rules.lang.identification;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ProgramIdParagraphContext;
import io.proleap.cobol.asg.metamodel.identification.ProgramIdParagraph;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ProgramIdParagraphRule extends CobolTransformRule<ProgramIdParagraphContext, ProgramIdParagraph> {

	@Override
	public void apply(final ProgramIdParagraphContext ctx, final ProgramIdParagraph programIdParagraph,
			final RuleContext rc) {
		rc.p("* Program-Id: %s", programIdParagraph.getName());
		rc.pNl(programIdParagraph);
	}

	@Override
	public Class<ProgramIdParagraphContext> from() {
		return ProgramIdParagraphContext.class;
	}
}
