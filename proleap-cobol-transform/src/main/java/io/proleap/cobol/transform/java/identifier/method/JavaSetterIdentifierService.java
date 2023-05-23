package io.proleap.cobol.transform.java.identifier.method;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;

public interface JavaSetterIdentifierService {

	String mapToIdentifier(DataDescriptionEntry dataDescriptionEntry);
}
