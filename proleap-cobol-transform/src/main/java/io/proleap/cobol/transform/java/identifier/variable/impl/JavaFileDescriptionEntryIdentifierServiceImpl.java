package io.proleap.cobol.transform.java.identifier.variable.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.transform.java.identifier.JavaIdentifierService;
import io.proleap.cobol.transform.java.identifier.variable.JavaFileDescriptionEntryIdentifierService;

@Singleton
public class JavaFileDescriptionEntryIdentifierServiceImpl implements JavaFileDescriptionEntryIdentifierService {

	@Inject
	private JavaIdentifierService javaIdentifierService;

	@Override
	public String mapToIdentifier(final FileDescriptionEntry fileDescriptionEntry) {
		return javaIdentifierService.mapToIdentifier(fileDescriptionEntry.getName()) + "Content";
	}
}
