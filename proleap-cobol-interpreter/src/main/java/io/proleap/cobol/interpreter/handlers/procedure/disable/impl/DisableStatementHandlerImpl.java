package io.proleap.cobol.interpreter.handlers.procedure.disable.impl;

import javax.annotation.PostConstruct;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.disable.DisableStatement;
import io.proleap.cobol.interpreter.handlers.procedure.disable.DisableStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class DisableStatementHandlerImpl extends StatementHandlerImpl<DisableStatement>
		implements DisableStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.DISABLE;
	}

	@Override
	public void run(final DisableStatement statement, final CobolInterpreterParams params) {
		// TODO messaging: implement MCS; The DISABLE statement notifies the Message
		// Control System (MCS) to inhibit data transfer between specified output queues
		// and destinations for output.
	}
}
