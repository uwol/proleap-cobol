package io.proleap.cobol.interpreter.handlers.data;

import io.proleap.cobol.asg.metamodel.data.DataDivision;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public interface DataDivisionHandler {

	void run(DataDivision dataDivision, CobolInterpreterParams params);
}
