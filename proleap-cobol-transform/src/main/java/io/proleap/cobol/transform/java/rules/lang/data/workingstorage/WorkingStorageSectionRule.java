package io.proleap.cobol.transform.java.rules.lang.data.workingstorage;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.WorkingStorageSectionContext;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.workingstorage.WorkingStorageSection;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class WorkingStorageSectionRule extends CobolTransformRule<WorkingStorageSectionContext, WorkingStorageSection> {

	@Override
	public void apply(final WorkingStorageSectionContext ctx, final WorkingStorageSection workingStorageSection,
			final RuleContext rc) {
		for (final DataDescriptionEntry dataDescriptionEntry : workingStorageSection.getRootDataDescriptionEntries()) {
			rc.visit(dataDescriptionEntry.getCtx());
		}
	}

	@Override
	public Class<WorkingStorageSectionContext> from() {
		return WorkingStorageSectionContext.class;
	}
}
