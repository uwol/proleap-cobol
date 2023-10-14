package io.proleap.cobol.interpreter.handlers.procedure.stop.impl;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.stop.StopStatement;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.stop.StopStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class StopStatementHandlerImpl extends StatementHandlerImpl<StopStatement> implements StopStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.STOP;
	}

	@Override
	public void run(final StopStatement statement, final CobolInterpreterParams params) {
		params.getState().setHalted(true);
	}
}
