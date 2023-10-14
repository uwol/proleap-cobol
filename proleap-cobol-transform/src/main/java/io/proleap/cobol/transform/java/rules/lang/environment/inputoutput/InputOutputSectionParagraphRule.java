package io.proleap.cobol.transform.java.rules.lang.environment.inputoutput;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.InputOutputSectionParagraphContext;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class InputOutputSectionParagraphRule extends CobolTransformRule<InputOutputSectionParagraphContext, ASGElement> {

	@Override
	public void apply(final InputOutputSectionParagraphContext ctx, final ASGElement asgElement, final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<InputOutputSectionParagraphContext> from() {
		return InputOutputSectionParagraphContext.class;
	}

}
