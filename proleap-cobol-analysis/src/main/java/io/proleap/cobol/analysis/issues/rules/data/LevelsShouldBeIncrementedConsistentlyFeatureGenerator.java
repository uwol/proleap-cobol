package io.proleap.cobol.analysis.issues.rules.data;

import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class LevelsShouldBeIncrementedConsistentlyFeatureGenerator extends FeatureGenerator<DataDescriptionEntry> {

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
		if (dataDescriptionEntry.getParentDataDescriptionEntryGroup() != null) {
			return parentIsRelevantDataDescriptionEntry(dataDescriptionEntry.getParentDataDescriptionEntryGroup());
		}

		return false;
	}

	protected boolean parentIsRelevantDataDescriptionEntry(final DataDescriptionEntry parentDataDescriptionEntry) {
		final DataDescriptionEntryGroup parentDataDescriptionEntryGroup = parentDataDescriptionEntry
				.getParentDataDescriptionEntryGroup();

		if (parentDataDescriptionEntryGroup == null) {
			return false;
		}

		final int levelNumber = parentDataDescriptionEntry.getLevelNumber();
		final int parentLevel = parentDataDescriptionEntryGroup.getLevelNumber();
		final boolean parentIsRoot = parentDataDescriptionEntryGroup.getParentDataDescriptionEntryGroup() == null;
		int childLevel = 0;

		if (DataDescriptionEntry.DataDescriptionEntryType.GROUP
				.equals(parentDataDescriptionEntry.getDataDescriptionEntryType())) {
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) parentDataDescriptionEntry;

			if (!dataDescriptionEntryGroup.getDataDescriptionEntries().isEmpty()) {
				childLevel = dataDescriptionEntryGroup.getDataDescriptionEntries().get(0).getLevelNumber();
			} else {
				return false;
			}
		} else {
			return false;
		}

		final int deltaParent = levelNumber - parentLevel;
		final int deltaChild = childLevel - levelNumber;

		if (deltaChild == deltaParent) {
			return false;
		} else if (parentIsRoot && levelNumber % 5 == 0 && childLevel % 5 == 0) {
			return false;
		}

		return true;
	}
}