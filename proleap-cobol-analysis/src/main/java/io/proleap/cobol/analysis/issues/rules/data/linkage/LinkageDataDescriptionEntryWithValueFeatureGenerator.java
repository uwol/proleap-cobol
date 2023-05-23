package io.proleap.cobol.analysis.issues.rules.data.linkage;

import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.ValueClause;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class LinkageDataDescriptionEntryWithValueFeatureGenerator extends FeatureGenerator<DataDescriptionEntry> {

	@Override
	public Stream<DataDescriptionEntry> getAll(final CompilationUnit compilationUnit) {
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
			final ValueClause valueClause = dataDescriptionEntryGroup.getValueClause();

			if (valueClause == null) {
				result = false;
			} else {
				result = true;
			}
		}

		return result;
	}
}
