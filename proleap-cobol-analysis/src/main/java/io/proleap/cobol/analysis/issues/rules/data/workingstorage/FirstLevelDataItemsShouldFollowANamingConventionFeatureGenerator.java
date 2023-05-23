package io.proleap.cobol.analysis.issues.rules.data.workingstorage;

import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class FirstLevelDataItemsShouldFollowANamingConventionFeatureGenerator
		extends FeatureGenerator<DataDescriptionEntry> {

	@Override
	public Stream<DataDescriptionEntry> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.workingStorageSectionDataDescriptionEntries(compilationUnit)
				.filter(dataDescriptionEntry -> {
					return isRelevantDataDescriptionEntry(dataDescriptionEntry);
				});
	}

	protected boolean isFirstLevelDataItem(final DataDescriptionEntry dataDescriptionEntry) {
		final boolean result;

		if (dataDescriptionEntry == null) {
			result = false;
		} else if (dataDescriptionEntry.getLevelNumber() != null && dataDescriptionEntry.getLevelNumber() != 1) {
			result = false;
		} else {
			result = true;
		}

		return result;
	}

	protected boolean isFollowingNamingConvention(final DataDescriptionEntry dataDescriptionEntry) {
		final boolean result;

		if (dataDescriptionEntry == null) {
			result = false;
		} else if (dataDescriptionEntry.getName() == null) {
			result = false;
		} else if (!dataDescriptionEntry.getName().matches("WS-.*")) {
			result = false;
		} else {
			result = true;
		}

		return result;
	}

	protected boolean isRelevantDataDescriptionEntry(final DataDescriptionEntry dataDescriptionEntry) {
		return isFirstLevelDataItem(dataDescriptionEntry) && !isFollowingNamingConvention(dataDescriptionEntry);
	}
}
