package io.proleap.cobol.transform.java.rules.lang.identification;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.InstallationParagraphContext;
import io.proleap.cobol.asg.metamodel.identification.InstallationParagraph;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class InstallationParagraphRule extends CobolTransformRule<InstallationParagraphContext, InstallationParagraph> {

	@Override
	public void apply(final InstallationParagraphContext ctx, final InstallationParagraph installationParagraph,
			final RuleContext rc) {
		rc.p("* Installed on: %s", installationParagraph.getInstallation());
		rc.pNl(installationParagraph);
	}

	@Override
	public Class<InstallationParagraphContext> from() {
		return InstallationParagraphContext.class;
	}
}
