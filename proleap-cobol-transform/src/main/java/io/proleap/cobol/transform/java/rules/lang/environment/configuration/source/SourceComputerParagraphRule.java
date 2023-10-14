package io.proleap.cobol.transform.java.rules.lang.environment.configuration.source;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.SourceComputerParagraphContext;
import io.proleap.cobol.asg.metamodel.environment.configuration.source.SourceComputerParagraph;
import io.proleap.cobol.transform.java.identifier.JavaIdentifierService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class SourceComputerParagraphRule extends CobolTransformRule<SourceComputerParagraphContext, SourceComputerParagraph> {

	@Inject
	private JavaIdentifierService javaIdentifierService;

	@Override
	public void apply(final SourceComputerParagraphContext ctx, final SourceComputerParagraph sourceComputerParagraph,
			final RuleContext rc) {
		rc.p("SourceComputer %sSourceComputer = new SourceComputer();",
				javaIdentifierService.mapToIdentifier(sourceComputerParagraph.getName()));
		rc.pNl(sourceComputerParagraph);
	}

	@Override
	public Class<SourceComputerParagraphContext> from() {
		return SourceComputerParagraphContext.class;
	}
}
