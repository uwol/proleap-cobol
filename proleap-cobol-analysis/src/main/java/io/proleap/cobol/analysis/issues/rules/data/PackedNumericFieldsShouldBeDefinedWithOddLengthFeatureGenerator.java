package io.proleap.cobol.analysis.issues.rules.data;

import java.util.stream.Stream;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.PictureClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.UsageClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.UsageClause.UsageClauseType;
import io.proleap.cobol.commons.datadescription.CobolPictureLengthService;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class PackedNumericFieldsShouldBeDefinedWithOddLengthFeatureGenerator
		extends FeatureGenerator<DataDescriptionEntry> {

	@Inject
	private CobolPictureLengthService cobolPictureLengthService;

	@Override
	public Stream<DataDescriptionEntry> getAll(final CompilationUnit compilationUnit) {
		final Stream<DataDescriptionEntry> linkageDataDescriptionEntries = CobolStreamUtils
				.linkageSectionDataDescriptionEntries(compilationUnit);
		final Stream<DataDescriptionEntry> workingStorageDataDescriptionEntries = CobolStreamUtils
				.workingStorageSectionDataDescriptionEntries(compilationUnit);
		final Stream<DataDescriptionEntry> dataDescriptionEntries = Stream.concat(linkageDataDescriptionEntries,
				workingStorageDataDescriptionEntries);

		return dataDescriptionEntries.filter(dataDescriptionEntry -> {
			return isRelevantDataDescriptionEntry(dataDescriptionEntry);
		});
	}

	protected boolean isRelevantDataDescriptionEntry(final DataDescriptionEntry dataDescriptionEntry) {
		final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry.getDataDescriptionEntryType();
		final boolean result;

		if (!DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)) {
			result = false;
		} else {
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
			final UsageClause usageClause = dataDescriptionEntryGroup.getUsageClause();

			if (usageClause == null) {
				result = false;
			} else {
				final UsageClauseType usageClauseType = usageClause.getUsageClauseType();

				if (!UsageClauseType.COMP_3.equals(usageClauseType)) {
					result = false;
				} else {
					final PictureClause pictureClause = dataDescriptionEntryGroup.getPictureClause();
					final String pictureString = pictureClause.getPictureString();
					final Integer integerPartLength = cobolPictureLengthService.getIntegerPartLength(pictureString);

					if (integerPartLength == null) {
						result = false;
					} else if ((integerPartLength & 1) != 0) {
						result = false;
					} else {
						result = true;
					}
				}
			}
		}

		return result;
	}
}
