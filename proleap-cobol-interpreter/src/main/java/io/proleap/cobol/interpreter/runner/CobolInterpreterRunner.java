package io.proleap.cobol.interpreter.runner;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public interface CobolInterpreterRunner {

	CobolState run(ProgramUnit programUnit, CobolInterpreterParams params);
}
