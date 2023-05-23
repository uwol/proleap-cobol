package io.proleap.cobol.transform.java.identifier.variable;

import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;

public interface JavaFileControlEntryIdentifierService {

	String mapToIdentifier(FileControlEntry fileControlEntry);
}
