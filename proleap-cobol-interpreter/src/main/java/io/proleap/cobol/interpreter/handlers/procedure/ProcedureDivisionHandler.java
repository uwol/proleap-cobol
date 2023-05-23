package io.proleap.cobol.interpreter.handlers.procedure;

import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public interface ProcedureDivisionHandler {

	void run(ProcedureDivision procedureDivision, CobolInterpreterParams params);
}
