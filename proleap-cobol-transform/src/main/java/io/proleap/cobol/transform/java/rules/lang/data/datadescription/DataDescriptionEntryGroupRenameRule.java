package io.proleap.cobol.transform.java.rules.lang.data.datadescription;

import java.util.List;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.DataDescriptionEntryFormat2Context;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryRename;
import io.proleap.cobol.asg.metamodel.data.datadescription.RenamesClause;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.transform.java.identifier.variable.JavaVariableIdentifierService;
import io.proleap.cobol.transform.java.type.JavaTypeService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class DataDescriptionEntryGroupRenameRule
		extends CobolTransformRule<DataDescriptionEntryFormat2Context, DataDescriptionEntryRename> {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private JavaTypeService javaTypeService;

	@Inject
	private JavaVariableIdentifierService javaVariableIdentifierService;

	@Override
	public void apply(final DataDescriptionEntryFormat2Context ctx,
			final DataDescriptionEntryRename dataDescriptionEntryRename, final RuleContext rc) {
		final RenamesClause renamesClause = dataDescriptionEntryRename.getRenamesClause();
		final Call call = renamesClause.getCalls().get(0);
		final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService.getDataDescriptionEntry(call);
		final String className = javaTypeService.mapToType(dataDescriptionEntry);
		final String identifier = javaVariableIdentifierService.mapToIdentifier(dataDescriptionEntryRename);

		rc.p("public %s %s = ", className, identifier);
		rc.visit(renamesClause.getFrom().getCtx());
		rc.p(";");
		rc.pNl(renamesClause);
	}

	@Override
	public Class<DataDescriptionEntryFormat2Context> from() {
		return DataDescriptionEntryFormat2Context.class;
	}

	@Override
	public Integer getPriority() {
		return 256;
	}

	@Override
	public boolean where(final DataDescriptionEntryFormat2Context ctx,
			final DataDescriptionEntryRename dataDescriptionEntryRename, final RuleContext rc) {
		final RenamesClause renamesClause = dataDescriptionEntryRename.getRenamesClause();
		final List<Call> calls = renamesClause.getCalls();
		final boolean isSingleCall = calls.size() == 1;

		final boolean result;

		if (!isSingleCall) {
			result = false;
		} else {
			final Call call = calls.get(0);
			final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService.getDataDescriptionEntry(call);

			if (dataDescriptionEntry == null) {
				result = false;
			} else {
				final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry
						.getDataDescriptionEntryType();

				if (!DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)) {
					result = false;
				} else {
					final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
					result = !dataDescriptionEntryGroup.getDataDescriptionEntries().isEmpty();
				}
			}
		}

		return result;
	}
}
