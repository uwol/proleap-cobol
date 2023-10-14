package io.proleap.cobol.transform.java.rules.lang.data.linkage;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.LinkageSectionContext;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.linkage.LinkageSection;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class LinkageSectionRule extends CobolTransformRule<LinkageSectionContext, LinkageSection> {

	@Override
	public void apply(final LinkageSectionContext ctx, final LinkageSection linkageSection, final RuleContext rc) {
		for (final DataDescriptionEntry dataDescriptionEntry : linkageSection.getRootDataDescriptionEntries()) {
			rc.visit(dataDescriptionEntry.getCtx());
		}
	}

	@Override
	public Class<LinkageSectionContext> from() {
		return LinkageSectionContext.class;
	}
}
