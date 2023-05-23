package io.proleap.cobol.transform.java.type;

import java.io.File;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.commons.type.CobolTypeEnum;

public interface JavaTypeService {

	String mapToType(CobolTypeEnum type);

	String mapToType(DataDescriptionEntry dataDescriptionEntry);

	String mapToType(DataDescriptionEntryGroup dataDescriptionEntryGroup);

	String mapToType(File inputFile);

	String mapToType(FileDescriptionEntry fileDescriptionEntry);

	String mapToType(String name);
}
