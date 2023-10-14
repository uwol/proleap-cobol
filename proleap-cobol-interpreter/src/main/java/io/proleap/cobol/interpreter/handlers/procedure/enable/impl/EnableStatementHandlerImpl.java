package io.proleap.cobol.interpreter.handlers.procedure.enable.impl;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.enable.EnableStatement;
import io.proleap.cobol.interpreter.handlers.procedure.enable.EnableStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class EnableStatementHandlerImpl extends StatementHandlerImpl<EnableStatement>
		implements EnableStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.ENABLE;
	}

	@Override
	public void run(final EnableStatement statement, final CobolInterpreterParams params) {
		// TODO messaging: implement MCS; The ENABLE statement notifies the Message
		// Control System (MCS) to allow data transfer between specified output queues
		// and destinations for output.
	}
}
