package io.proleap.cobol.analysis.issues.rules.data;

import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class Level77ShouldNotBeUsedFeatureGenerator extends FeatureGenerator<DataDescriptionEntry> {

	@Override
	public Stream<DataDescriptionEntry> getAll(final CompilationUnit compilationUnit) {
		return Stream.concat(getWorkingStorageDataDescriptionEntries(compilationUnit),
				getLinkageSectionDataDescriptionEntries(compilationUnit));
	}

	protected Stream<DataDescriptionEntry> getLinkageSectionDataDescriptionEntries(
			final CompilationUnit compilationUnit) {
		final Stream<DataDescriptionEntry> dataDescriptionEntries = CobolStreamUtils
				.linkageSectionDataDescriptionEntries(compilationUnit);

		return dataDescriptionEntries.filter(dataDescriptionEntry -> {
			return isRelevantDataDescriptionEntry(dataDescriptionEntry);
		});
	}

	protected Stream<DataDescriptionEntry> getWorkingStorageDataDescriptionEntries(
			final CompilationUnit compilationUnit) {
		final Stream<DataDescriptionEntry> dataDescriptionEntries = CobolStreamUtils
				.workingStorageSectionDataDescriptionEntries(compilationUnit);

		return dataDescriptionEntries.filter(dataDescriptionEntry -> {
			return isRelevantDataDescriptionEntry(dataDescriptionEntry);
		});
	}

	protected boolean isRelevantDataDescriptionEntry(final DataDescriptionEntry dataDescriptionEntry) {
		final Integer levelNumber = dataDescriptionEntry.getLevelNumber();
		return levelNumber != null && levelNumber.equals(DataDescriptionEntry.LEVEL_NUMBER_SCALAR);
	}
}
