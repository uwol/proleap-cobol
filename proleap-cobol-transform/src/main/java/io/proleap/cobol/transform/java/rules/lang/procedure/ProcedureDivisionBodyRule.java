package io.proleap.cobol.transform.java.rules.lang.procedure;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ProcedureDivisionBodyContext;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ProcedureDivisionBodyRule extends CobolTransformRule<ProcedureDivisionBodyContext, ASGElement> {

	@Override
	public void apply(final ProcedureDivisionBodyContext ctx, final ASGElement semanticGraphElement,
			final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<ProcedureDivisionBodyContext> from() {
		return ProcedureDivisionBodyContext.class;
	}
}
