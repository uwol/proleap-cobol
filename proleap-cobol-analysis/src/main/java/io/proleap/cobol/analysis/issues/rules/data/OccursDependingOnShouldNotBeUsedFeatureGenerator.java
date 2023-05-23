package io.proleap.cobol.analysis.issues.rules.data;

import java.util.List;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.OccursClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.OccursDepending;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class OccursDependingOnShouldNotBeUsedFeatureGenerator extends FeatureGenerator<DataDescriptionEntry> {
	@Override
	public Stream<DataDescriptionEntry> getAll(final CompilationUnit compilationUnit) {
		return Stream.concat(getWorkingStorageDataDescriptionEntries(compilationUnit),
				getLinkageSectionDataDescriptionEntries(compilationUnit));
	}

	protected Stream<DataDescriptionEntry> getLinkageSectionDataDescriptionEntries(
			final CompilationUnit compilationUnit) {
		return CobolStreamUtils.linkageSectionDataDescriptionEntries(compilationUnit).filter(dataDescriptionEntry -> {
			return isRelevantDataDescriptionEntry(dataDescriptionEntry);
		});
	}

	protected Stream<DataDescriptionEntry> getWorkingStorageDataDescriptionEntries(
			final CompilationUnit compilationUnit) {
		return CobolStreamUtils.workingStorageSectionDataDescriptionEntries(compilationUnit)
				.filter(dataDescriptionEntry -> {
					return isRelevantDataDescriptionEntry(dataDescriptionEntry);
				});
	}

	protected boolean isRelevantDataDescriptionEntry(final DataDescriptionEntry dataDescriptionEntry) {
		if (!DataDescriptionEntry.DataDescriptionEntryType.GROUP
				.equals(dataDescriptionEntry.getDataDescriptionEntryType())) {
			return false;
		}

		final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
		final List<OccursClause> occursClauses = dataDescriptionEntryGroup.getOccursClauses();

		for (final OccursClause clause : occursClauses) {
			final OccursDepending occursDepending = clause.getOccursDepending();

			if (occursDepending != null) {
				final Call dependingOnCall = occursDepending.getDependingOnCall();

				if (dependingOnCall != null) {
					return true;
				}
			}
		}

		return false;
	}
}
