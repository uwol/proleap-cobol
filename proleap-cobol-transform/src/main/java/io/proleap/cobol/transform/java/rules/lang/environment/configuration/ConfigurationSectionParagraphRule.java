package io.proleap.cobol.transform.java.rules.lang.environment.configuration;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ConfigurationSectionParagraphContext;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ConfigurationSectionParagraphRule extends CobolTransformRule<ConfigurationSectionParagraphContext, ASGElement> {

	@Override
	public void apply(final ConfigurationSectionParagraphContext ctx, final ASGElement configurationSection,
			final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<ConfigurationSectionParagraphContext> from() {
		return ConfigurationSectionParagraphContext.class;
	}
}
