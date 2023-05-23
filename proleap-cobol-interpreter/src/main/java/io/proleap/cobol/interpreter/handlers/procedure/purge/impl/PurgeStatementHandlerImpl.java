package io.proleap.cobol.interpreter.handlers.procedure.purge.impl;

import javax.annotation.PostConstruct;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.purge.PurgeStatement;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.purge.PurgeStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class PurgeStatementHandlerImpl extends StatementHandlerImpl<PurgeStatement> implements PurgeStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.PURGE;
	}

	@Override
	public void run(final PurgeStatement statement, final CobolInterpreterParams params) {
		// TODO messaging: implement MCS; eliminates from the Message Control System
		// (MCS) a partial message that has been released by one or more SEND
		// statements.
	}
}
