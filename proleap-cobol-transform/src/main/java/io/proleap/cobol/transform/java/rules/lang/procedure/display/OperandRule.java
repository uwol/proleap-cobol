package io.proleap.cobol.transform.java.rules.lang.procedure.display;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.DisplayOperandContext;
import io.proleap.cobol.asg.metamodel.procedure.display.Operand;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class OperandRule extends CobolTransformRule<DisplayOperandContext, Operand> {

	@Override
	public void apply(final DisplayOperandContext ctx, final Operand operand, final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<DisplayOperandContext> from() {
		return DisplayOperandContext.class;
	}
}
