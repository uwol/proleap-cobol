package io.proleap.cobol.transform.java.identifier.variable.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.RedefinesClause;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.transform.java.identifier.JavaIdentifierService;
import io.proleap.cobol.transform.java.identifier.variable.JavaVariableIdentifierService;

@Singleton
public class JavaVariableIdentifierServiceImpl implements JavaVariableIdentifierService {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Inject
	private JavaIdentifierService javaIdentifierService;

	@Override
	public String mapToIdentifier(final DataDescriptionEntry dataDescriptionEntry) {
		final String result;

		if (dataDescriptionEntry instanceof DataDescriptionEntryGroup) {
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
			result = mapToIdentifier(dataDescriptionEntryGroup);
		} else {
			result = javaIdentifierService.mapToIdentifier(dataDescriptionEntry.getName());
		}

		return result;
	}

	@Override
	public String mapToIdentifier(final DataDescriptionEntryGroup dataDescriptionEntryGroup) {
		final Boolean isFiller = dataDescriptionEntryGroup.getFiller();
		final RedefinesClause redefinesClause = dataDescriptionEntryGroup.getRedefinesClause();
		final String result;

		if (Boolean.TRUE.equals(isFiller)) {
			result = javaIdentifierService.mapToIdentifier("filler" + dataDescriptionEntryGroup.getFillerNumber());
		} else if (redefinesClause != null) {
			cobolDataDescriptionEntryService.getDataDescriptionEntry(redefinesClause.getRedefinesCall());
			result = javaIdentifierService.mapToIdentifier(dataDescriptionEntryGroup.getName());
		} else {
			result = javaIdentifierService.mapToIdentifier(dataDescriptionEntryGroup.getName());
		}

		return result;
	}
}
