package io.proleap.cobol.transform.java.identifier.variable;

import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;

public interface JavaFileDescriptionEntryIdentifierService {

	String mapToIdentifier(FileDescriptionEntry fileDescriptionEntry);
}
