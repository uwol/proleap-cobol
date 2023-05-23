package io.proleap.cobol.transform.java.type;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;

public interface JavaInstanceService {

	String mapToInstance(DataDescriptionEntryGroup dataDescriptionEntryGroup);
}
