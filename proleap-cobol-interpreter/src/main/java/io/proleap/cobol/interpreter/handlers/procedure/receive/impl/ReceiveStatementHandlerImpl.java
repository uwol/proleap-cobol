package io.proleap.cobol.interpreter.handlers.procedure.receive.impl;

import javax.annotation.PostConstruct;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.receive.ReceiveStatement;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.receive.ReceiveStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class ReceiveStatementHandlerImpl extends StatementHandlerImpl<ReceiveStatement>
		implements ReceiveStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.RECEIVE;
	}

	@Override
	public void run(final ReceiveStatement statement, final CobolInterpreterParams params) {
		// TODO messaging: implement MCS; stop thread and start on receiving message
		// from outside the program
	}
}
