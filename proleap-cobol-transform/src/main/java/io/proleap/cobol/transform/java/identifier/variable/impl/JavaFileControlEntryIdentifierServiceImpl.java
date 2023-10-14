package io.proleap.cobol.transform.java.identifier.variable.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.transform.java.identifier.JavaIdentifierService;
import io.proleap.cobol.transform.java.identifier.variable.JavaFileControlEntryIdentifierService;

@Singleton
public class JavaFileControlEntryIdentifierServiceImpl implements JavaFileControlEntryIdentifierService {

	@Inject
	private JavaIdentifierService javaIdentifierService;

	@Override
	public String mapToIdentifier(final FileControlEntry fileControlEntry) {
		return javaIdentifierService.mapToIdentifier(fileControlEntry.getName());
	}
}
