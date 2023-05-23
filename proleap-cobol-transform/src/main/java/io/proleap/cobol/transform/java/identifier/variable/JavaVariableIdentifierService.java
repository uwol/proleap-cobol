package io.proleap.cobol.transform.java.identifier.variable;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;

public interface JavaVariableIdentifierService {

	String mapToIdentifier(DataDescriptionEntry dataDescriptionEntry);

	String mapToIdentifier(DataDescriptionEntryGroup dataDescriptionEntryGroup);
}
