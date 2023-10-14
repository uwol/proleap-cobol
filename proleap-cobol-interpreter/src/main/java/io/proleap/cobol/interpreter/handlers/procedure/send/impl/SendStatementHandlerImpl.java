package io.proleap.cobol.interpreter.handlers.procedure.send.impl;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.send.SendStatement;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.send.SendStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class SendStatementHandlerImpl extends StatementHandlerImpl<SendStatement> implements SendStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.SEND;
	}

	@Override
	public void run(final SendStatement statement, final CobolInterpreterParams params) {
		// TODO messaging: implement MCS; The SEND MESSAGE statement sends a message to
		// another terminal or user, or to a group of terminals or users that is defined
		// as a destination, during system generation.
	}
}
