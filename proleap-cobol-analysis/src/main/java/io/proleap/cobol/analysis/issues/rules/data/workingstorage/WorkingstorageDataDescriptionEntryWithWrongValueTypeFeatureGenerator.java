package io.proleap.cobol.analysis.issues.rules.data.workingstorage;

import java.util.stream.Stream;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.PictureClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.ValueClause;
import io.proleap.cobol.commons.type.CobolTypeEnum;
import io.proleap.cobol.commons.type.CobolTypeService;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class WorkingstorageDataDescriptionEntryWithWrongValueTypeFeatureGenerator
		extends FeatureGenerator<DataDescriptionEntry> {

	@Inject
	private CobolTypeService cobolTypeService;

	@Override
	public Stream<DataDescriptionEntry> getAll(final CompilationUnit compilationUnit) {
		final Stream<DataDescriptionEntry> dataDescriptionEntries = CobolStreamUtils
				.workingStorageSectionDataDescriptionEntries(compilationUnit);
		return dataDescriptionEntries.filter(dataDescriptionEntry -> {
			return isValueTypeDifferentThanPictureType(dataDescriptionEntry);
		});
	}

	protected boolean isDifferentType(final CobolTypeEnum pictureType, final CobolTypeEnum valueType) {
		final boolean result;

		if (CobolTypeEnum.BOOLEAN.equals(pictureType)) {
			if (CobolTypeEnum.BOOLEAN.equals(valueType)) {
				result = false;
			} else {
				result = true;
			}
		} else if (CobolTypeEnum.STRING.equals(pictureType)) {
			if (CobolTypeEnum.STRING.equals(valueType)) {
				result = false;
			} else {
				result = true;
			}
		} else if (CobolTypeEnum.INTEGER.equals(pictureType)) {
			if (CobolTypeEnum.INTEGER.equals(valueType)) {
				result = false;
			} else {
				result = true;
			}
		} else if (CobolTypeEnum.FLOAT.equals(pictureType)) {
			if (CobolTypeEnum.FLOAT.equals(valueType)) {
				result = false;
			} else if (CobolTypeEnum.INTEGER.equals(valueType)) {
				result = false;
			} else {
				result = true;
			}
		} else {
			result = false;
		}

		return result;
	}

	protected boolean isValueTypeDifferentThanPictureType(final DataDescriptionEntry dataDescriptionEntry) {
		final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry.getDataDescriptionEntryType();
		final boolean result;

		if (DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)) {
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
			final PictureClause pictureClause = dataDescriptionEntryGroup.getPictureClause();
			final ValueClause valueClause = dataDescriptionEntryGroup.getValueClause();

			if (pictureClause == null) {
				result = false;
			} else if (valueClause == null) {
				result = false;
			} else {
				final CobolTypeEnum pictureType = cobolTypeService.getType(pictureClause);
				final CobolTypeEnum valueType = cobolTypeService.getType(valueClause);

				if (pictureType == null) {
					result = false;
				} else if (valueType == null) {
					result = false;
				} else {
					result = isDifferentType(pictureType, valueType);
				}
			}
		} else {
			result = false;
		}

		return result;
	}
}
