package io.proleap.cobol.transform.java.rules.lang.identification;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.RemarksParagraphContext;
import io.proleap.cobol.asg.metamodel.identification.RemarksParagraph;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class RemarksParagraphRule extends CobolTransformRule<RemarksParagraphContext, RemarksParagraph> {

	@Override
	public void apply(final RemarksParagraphContext ctx, final RemarksParagraph remarksParagraph,
			final RuleContext rc) {
		rc.p("* Remarks: %s", remarksParagraph.getRemarks());
		rc.pNl(remarksParagraph);
	}

	@Override
	public Class<RemarksParagraphContext> from() {
		return RemarksParagraphContext.class;
	}
}
