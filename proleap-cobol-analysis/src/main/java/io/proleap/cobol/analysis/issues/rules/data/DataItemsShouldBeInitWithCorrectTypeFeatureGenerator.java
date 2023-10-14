package io.proleap.cobol.analysis.issues.rules.data;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryCondition;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.PictureClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.ValueClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.ValueInterval;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.type.CobolTypeEnum;
import io.proleap.cobol.commons.type.CobolTypeService;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class DataItemsShouldBeInitWithCorrectTypeFeatureGenerator extends FeatureGenerator<DataDescriptionEntry> {

	@Inject
	private CobolTypeService cobolTypeService;

	protected boolean correctType(final PictureClause pictureClause, final ValueClause valueClause) {
		final Set<CobolTypeEnum> typeSet = new HashSet<>();
		final List<ValueInterval> valueIntervals = valueClause.getValueIntervals();
		final CobolTypeEnum pictureType = cobolTypeService.getType(pictureClause);

		final boolean result;

		if (pictureType == null) {
			result = false;
		} else {
			typeSet.add(pictureType);

			for (final ValueInterval valueInterval : valueIntervals) {
				final ValueStmt fromValueStmt = valueInterval.getFromValueStmt();
				final ValueStmt toValueStmt = valueInterval.getToValueStmt();

				if (fromValueStmt != null) {
					final CobolTypeEnum type = cobolTypeService.getType(fromValueStmt);
					typeSet.add(type);
				}
				if (toValueStmt != null) {
					final CobolTypeEnum type = cobolTypeService.getType(toValueStmt);
					typeSet.add(type);
				}
			}

			if (typeSet.size() == 1) {
				result = false;
			} else {
				result = true;
			}
		}

		return result;
	}

	@Override
	public Stream<DataDescriptionEntry> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.dataDescriptionEntriesRec(compilationUnit)
				.filter(dataDescriptionEntry -> relevantDataDescriptionEntry(dataDescriptionEntry));
	}

	protected boolean relevantDataDescriptionEntry(final DataDescriptionEntry dataDescriptionEntry) {
		final boolean result;
		final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry.getDataDescriptionEntryType();

		if (DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)
				|| DataDescriptionEntryType.SCALAR.equals(dataDescriptionEntryType)) {
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
			final PictureClause pictureClause = dataDescriptionEntryGroup.getPictureClause();
			final ValueClause valueClause = dataDescriptionEntryGroup.getValueClause();

			if (valueClause == null || pictureClause == null) {
				result = false;
			} else {
				result = correctType(pictureClause, valueClause);
			}
		} else if (DataDescriptionEntryType.CONDITION.equals(dataDescriptionEntryType)) {
			final DataDescriptionEntryCondition dataDescriptionEntryCondition = (DataDescriptionEntryCondition) dataDescriptionEntry;
			final ValueClause valueClause = dataDescriptionEntryCondition.getValueClause();
			final DataDescriptionEntryGroup parentDataDescriptionEntryGroup = dataDescriptionEntry
					.getParentDataDescriptionEntryGroup();
			final PictureClause pictureClause = parentDataDescriptionEntryGroup.getPictureClause();

			if (valueClause == null || pictureClause == null) {
				result = false;
			} else {
				result = correctType(pictureClause, valueClause);
			}
		} else {
			result = false;
		}

		return result;
	}
}
