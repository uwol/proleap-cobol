package io.proleap.cobol.transform.java.identifier.method.impl;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.util.StringUtils;
import io.proleap.cobol.transform.java.identifier.method.JavaSetterIdentifierService;
import io.proleap.cobol.transform.java.identifier.variable.JavaVariableIdentifierService;

@Singleton
public class JavaSetterIdentifierServiceImpl implements JavaSetterIdentifierService {

	@Inject
	private JavaVariableIdentifierService javaVariableIdentifierService;

	@Override
	public String mapToIdentifier(final DataDescriptionEntry dataDescriptionEntry) {
		final String variableIdentifier = javaVariableIdentifierService.mapToIdentifier(dataDescriptionEntry);
		return variableIdentifier == null ? null : "set" + StringUtils.capitalize(variableIdentifier);
	}
}
