package io.proleap.cobol.transform.java.rules.lang.environment.configuration.object;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ObjectComputerParagraphContext;
import io.proleap.cobol.asg.metamodel.environment.configuration.object.ObjectComputerParagraph;
import io.proleap.cobol.transform.java.identifier.JavaIdentifierService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ObjectComputerParagraphRule extends CobolTransformRule<ObjectComputerParagraphContext, ObjectComputerParagraph> {

	@Inject
	private JavaIdentifierService javaIdentifierService;

	@Override
	public void apply(final ObjectComputerParagraphContext ctx, final ObjectComputerParagraph objectComputerParagraph,
			final RuleContext rc) {
		rc.p("ObjectComputer %sObjectComputer = new ObjectComputer();",
				javaIdentifierService.mapToIdentifier(objectComputerParagraph.getName()));
		rc.pNl(objectComputerParagraph);
	}

	@Override
	public Class<ObjectComputerParagraphContext> from() {
		return ObjectComputerParagraphContext.class;
	}
}
