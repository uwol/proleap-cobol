package io.proleap.cobol.interpreter.handlers.impl;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.DataDivision;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.interpreter.handlers.ProgramUnitHandler;
import io.proleap.cobol.interpreter.handlers.data.DataDivisionHandler;
import io.proleap.cobol.interpreter.handlers.procedure.ProcedureDivisionHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class ProgramUnitHandlerImpl extends AbstractCobolInterpreterHandler implements ProgramUnitHandler {

	@Inject
	private DataDivisionHandler dataDivisionHandler;

	@Inject
	private ProcedureDivisionHandler procedureDivisionHandler;

	@Override
	public void run(final ProgramUnit programUnit, final CobolInterpreterParams params) {
		final DataDivision dataDivision = programUnit.getDataDivision();

		if (dataDivision != null) {
			dataDivisionHandler.run(dataDivision, params);
		}

		final ProcedureDivision procedureDivision = programUnit.getProcedureDivision();

		if (procedureDivision != null) {
			procedureDivisionHandler.run(procedureDivision, params);
		}
	}
}
