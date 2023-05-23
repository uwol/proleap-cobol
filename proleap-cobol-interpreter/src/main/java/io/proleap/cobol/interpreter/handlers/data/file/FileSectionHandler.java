package io.proleap.cobol.interpreter.handlers.data.file;

import io.proleap.cobol.asg.metamodel.data.file.FileSection;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public interface FileSectionHandler {

	void run(FileSection fileSection, CobolInterpreterParams params);
}
