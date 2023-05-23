package io.proleap.cobol.interpreter.handlers.procedure;

import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public interface SectionHandler {

	void run(Section section, CobolInterpreterParams params);
}
