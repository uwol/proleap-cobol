package io.proleap.cobol.interpreter.handlers.procedure.execcics.impl;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.execcics.ExecCicsStatement;
import io.proleap.cobol.interpreter.handlers.procedure.execcics.ExecCicsStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class ExecCicsStatementHandlerImpl extends StatementHandlerImpl<ExecCicsStatement>
		implements ExecCicsStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.EXEC_CICS;
	}

	@Override
	public void run(final ExecCicsStatement statement, final CobolInterpreterParams params) {
		// TODO integrate CICS
	}
}
