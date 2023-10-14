package io.proleap.cobol.transform.java.rules.lang.environment.configuration;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ConfigurationSectionContext;
import io.proleap.cobol.asg.metamodel.environment.configuration.ConfigurationSection;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ConfigurationSectionRule extends CobolTransformRule<ConfigurationSectionContext, ConfigurationSection> {

	@Override
	public void apply(final ConfigurationSectionContext ctx, final ConfigurationSection configurationSection,
			final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<ConfigurationSectionContext> from() {
		return ConfigurationSectionContext.class;
	}
}
