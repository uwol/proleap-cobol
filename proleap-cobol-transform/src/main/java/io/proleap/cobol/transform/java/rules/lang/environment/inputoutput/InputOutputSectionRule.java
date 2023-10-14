package io.proleap.cobol.transform.java.rules.lang.environment.inputoutput;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.InputOutputSectionContext;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.InputOutputSection;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class InputOutputSectionRule extends CobolTransformRule<InputOutputSectionContext, InputOutputSection> {

	@Override
	public void apply(final InputOutputSectionContext ctx, final InputOutputSection inputOutputSection,
			final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<InputOutputSectionContext> from() {
		return InputOutputSectionContext.class;
	}
}
