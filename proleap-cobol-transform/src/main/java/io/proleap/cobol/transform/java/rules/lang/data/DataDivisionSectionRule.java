package io.proleap.cobol.transform.java.rules.lang.data;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.DataDivisionSectionContext;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class DataDivisionSectionRule extends CobolTransformRule<DataDivisionSectionContext, ASGElement> {

	@Override
	public void apply(final DataDivisionSectionContext ctx, final ASGElement asgElement, final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<DataDivisionSectionContext> from() {
		return DataDivisionSectionContext.class;
	}
}
