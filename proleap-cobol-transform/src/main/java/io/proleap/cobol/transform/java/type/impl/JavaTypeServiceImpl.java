package io.proleap.cobol.transform.java.type.impl;

import java.io.File;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.util.FilenameUtils;
import io.proleap.cobol.asg.util.StringUtils;
import io.proleap.cobol.commons.type.CobolTypeEnum;
import io.proleap.cobol.commons.type.CobolTypeService;
import io.proleap.cobol.transform.java.type.JavaTypeEnum;
import io.proleap.cobol.transform.java.type.JavaTypeService;

@Singleton
public class JavaTypeServiceImpl implements JavaTypeService {

	@Inject
	private CobolTypeService cobolTypeService;

	@Override
	public String mapToType(final CobolTypeEnum type) {
		final String result;

		if (type == null) {
			result = "Object";
		} else if (CobolTypeEnum.BOOLEAN.equals(type)) {
			result = JavaTypeEnum.BOOLEAN.getName();
		} else if (CobolTypeEnum.FLOAT.equals(type)) {
			result = JavaTypeEnum.BIGDECIMAL.getName();
		} else if (CobolTypeEnum.INTEGER.equals(type)) {
			result = JavaTypeEnum.BIGDECIMAL.getName();
		} else if (CobolTypeEnum.STRING.equals(type)) {
			result = JavaTypeEnum.STRING.getName();
		} else {
			result = "Object";
		}

		return result;
	}

	@Override
	public String mapToType(final DataDescriptionEntry dataDescriptionEntry) {
		final String result;

		if (dataDescriptionEntry instanceof DataDescriptionEntryGroup) {
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
			result = mapToType(dataDescriptionEntryGroup);
		} else {
			return mapToType(dataDescriptionEntry.getName()) + "Type";
		}

		return result;
	}

	@Override
	public String mapToType(final DataDescriptionEntryGroup dataDescriptionEntryGroup) {
		final CobolTypeEnum type = cobolTypeService.getType(dataDescriptionEntryGroup);
		final String result;

		if (type == null) {
			result = "Object";
		} else {
			switch (type) {
			case DATA_DESCRIPTION_GROUP:
				final Boolean isFiller = dataDescriptionEntryGroup.getFiller();

				if (Boolean.TRUE.equals(isFiller)) {
					result = mapToType("Filler" + dataDescriptionEntryGroup.getFillerNumber()) + "Type";
				} else {
					result = mapToType(dataDescriptionEntryGroup.getName()) + "Type";
				}
				break;
			case BOOLEAN:
			case FLOAT:
			case INTEGER:
			case STRING:
			default:
				result = mapToType(type);
				break;
			}
		}

		return result;
	}

	@Override
	public String mapToType(final File inputFile) {
		return mapToType(FilenameUtils.removeExtension(inputFile.getName()));
	}

	@Override
	public String mapToType(final FileDescriptionEntry fileDescriptionEntry) {
		return mapToType(fileDescriptionEntry.getName()) + "Type";
	}

	@Override
	public String mapToType(final String name) {
		final String result;

		if (name == null || name.isEmpty()) {
			result = name;
		} else if (name.contains("-") || name.contains("_")) {
			result = name.toLowerCase().replace("-", "_");
		} else {
			result = name;
		}

		return StringUtils.capitalize(result);
	}
}
