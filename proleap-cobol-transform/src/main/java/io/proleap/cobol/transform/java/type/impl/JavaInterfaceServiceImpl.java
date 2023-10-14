package io.proleap.cobol.transform.java.type.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.commons.datadescription.CobolPictureStringService;
import io.proleap.cobol.transform.java.type.JavaInterfaceService;
import io.proleap.cobol.transform.java.type.JavaTypeService;

@Singleton
public class JavaInterfaceServiceImpl implements JavaInterfaceService {

	@Inject
	private JavaTypeService javaTypeService;

	@Inject
	private CobolPictureStringService pictureStringService;

	@Override
	public String mapToInterface(final DataDescriptionEntryGroup dataDescriptionEntryGroup) {
		final String typeName = javaTypeService.mapToType(dataDescriptionEntryGroup);
		final int numberOfOccurrs = pictureStringService.getMaxOccurs(dataDescriptionEntryGroup);
		final String result;

		if (numberOfOccurrs > 1) {
			result = String.format("List<%s>", typeName);
		} else {
			result = typeName;
		}

		return result;
	}
}
