package io.proleap.cobol.interpreter.runner.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.handlers.ProgramUnitHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.runner.CobolInterpreterRunner;

@Singleton
public class CobolInterpreterRunnerImpl implements CobolInterpreterRunner {

	@Inject
	protected ProgramUnitHandler programUnitHandler;

	@Override
	public CobolState run(final ProgramUnit programUnit, final CobolInterpreterParams params) {
		programUnitHandler.run(programUnit, params);
		return params.getState();
	}
}
