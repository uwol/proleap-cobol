package io.proleap.cobol.analysis.issues.rules.data;

import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.UsageClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.UsageClause.UsageClauseType;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class ComputationalShouldNotBeUsedFeatureGenerator extends FeatureGenerator<DataDescriptionEntry> {

	@Override
	public Stream<DataDescriptionEntry> getAll(final CompilationUnit compilationUnit) {
		return Stream.concat(getWSDataDescriptionEntries(compilationUnit),
				getLSDataDescriptionEntries(compilationUnit));
	}

	protected Stream<DataDescriptionEntry> getLSDataDescriptionEntries(final CompilationUnit compilationUnit) {
		final Stream<DataDescriptionEntry> dataDescriptionEntries = CobolStreamUtils
				.workingStorageSectionDataDescriptionEntries(compilationUnit);
		return dataDescriptionEntries.filter(dataDescriptionEntry -> {
			return isRelevantDataDescriptionEntry(dataDescriptionEntry);
		});
	}

	protected Stream<DataDescriptionEntry> getWSDataDescriptionEntries(final CompilationUnit compilationUnit) {
		final Stream<DataDescriptionEntry> dataDescriptionEntries = CobolStreamUtils
				.linkageSectionDataDescriptionEntries(compilationUnit);
		return dataDescriptionEntries.filter(dataDescriptionEntry -> {
			return isRelevantDataDescriptionEntry(dataDescriptionEntry);
		});
	}

	protected boolean isRelevantDataDescriptionEntry(final DataDescriptionEntry dataDescriptionEntry) {
		final boolean isGroup = DataDescriptionEntryType.GROUP
				.equals(dataDescriptionEntry.getDataDescriptionEntryType());
		final boolean result;

		if (!isGroup) {
			result = false;
		} else {
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
			final UsageClause usageClause = dataDescriptionEntryGroup.getUsageClause();

			if (usageClause == null) {
				result = false;
			} else {
				final UsageClauseType usageClauseType = usageClause.getUsageClauseType();

				if (!isRelevantUsageClause(usageClauseType)) {
					result = false;
				} else {
					result = true;
				}
			}
		}

		return result;
	}

	protected boolean isRelevantUsageClause(final UsageClauseType usageClauseType) {
		final boolean result;

		switch (usageClauseType) {
		case COMP:
			result = true;
			break;
		case COMP_1:
			result = true;
			break;
		case COMP_2:
			result = true;
			break;
		case COMP_3:
			result = true;
			break;
		case COMP_4:
			result = true;
			break;
		case COMP_5:
			result = false;
			break;
		default:
			result = false;
			break;
		}

		return result;
	}
}
