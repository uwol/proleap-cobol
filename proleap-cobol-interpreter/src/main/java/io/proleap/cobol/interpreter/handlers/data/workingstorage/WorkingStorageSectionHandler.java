package io.proleap.cobol.interpreter.handlers.data.workingstorage;

import io.proleap.cobol.asg.metamodel.data.workingstorage.WorkingStorageSection;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public interface WorkingStorageSectionHandler {

	void run(WorkingStorageSection workingStorageSection, CobolInterpreterParams params);
}
