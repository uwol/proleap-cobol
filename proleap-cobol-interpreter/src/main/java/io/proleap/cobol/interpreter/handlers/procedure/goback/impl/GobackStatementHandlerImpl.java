package io.proleap.cobol.interpreter.handlers.procedure.goback.impl;

import javax.annotation.PostConstruct;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.goback.GobackStatement;
import io.proleap.cobol.interpreter.handlers.procedure.goback.GobackStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class GobackStatementHandlerImpl extends StatementHandlerImpl<GobackStatement>
		implements GobackStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.GO_BACK;
	}

	@Override
	public void run(final GobackStatement statement, final CobolInterpreterParams params) {
		params.getState().setHalted(true);
	}
}
