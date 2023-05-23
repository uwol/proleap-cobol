package io.proleap.cobol.analysis.issues.rules.data;

import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.TypeDefClause;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class NewTypesShouldBeDefinedInCopybooksFeatureGenerator extends FeatureGenerator<DataDescriptionEntry> {

	@Override
	public Stream<DataDescriptionEntry> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.dataDescriptionEntriesRec(compilationUnit)
				.filter(dataDescriptionEntry -> isRelevantDataDescriptionEntry(dataDescriptionEntry));
	}

	protected boolean isRelevantDataDescriptionEntry(final DataDescriptionEntry dataDescriptionEntry) {
		final boolean isGroup = DataDescriptionEntryType.GROUP
				.equals(dataDescriptionEntry.getDataDescriptionEntryType());
		final boolean result;

		if (!isGroup) {
			result = false;
		} else {
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
			final TypeDefClause typeDefClause = dataDescriptionEntryGroup.getTypeDefClause();

			if (typeDefClause == null) {
				result = false;
			} else {
				// TODO: Filter typeDefClauses that are in a copybook.
				result = true;
			}
		}

		return result;
	}
}
