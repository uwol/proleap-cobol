package io.proleap.cobol.interpreter.handlers;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public interface ProgramUnitHandler {

	void run(ProgramUnit programUnit, CobolInterpreterParams params);
}
