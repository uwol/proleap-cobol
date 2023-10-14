package io.proleap.cobol.transform.java.rules.lang.data.datadescription;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.DataDescriptionEntryContext;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class DataDescriptionEntryRule extends CobolTransformRule<DataDescriptionEntryContext, ASGElement> {

	@Override
	public void apply(final DataDescriptionEntryContext ctx, final ASGElement asgElement, final RuleContext rc) {
		rc.visitChildren(ctx);
	}

	@Override
	public Class<DataDescriptionEntryContext> from() {
		return DataDescriptionEntryContext.class;
	}
}
