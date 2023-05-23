package io.proleap.cobol.interpreter.handlers.procedure.terminate.impl;

import javax.annotation.PostConstruct;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.terminate.TerminateStatement;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.terminate.TerminateStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class TerminateStatementHandlerImpl extends StatementHandlerImpl<TerminateStatement>
		implements TerminateStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.TERMINATE;
	}

	@Override
	public void run(final TerminateStatement statement, final CobolInterpreterParams params) {
	}
}
