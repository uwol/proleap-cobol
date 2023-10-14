package io.proleap.cobol.transform.java.rules.lang;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ProgramUnitContext;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ProgramUnitRule extends CobolTransformRule<ProgramUnitContext, ProgramUnit> {

	@Override
	public void apply(final ProgramUnitContext ctx, final ProgramUnit programUnit, final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<ProgramUnitContext> from() {
		return ProgramUnitContext.class;
	}
}
