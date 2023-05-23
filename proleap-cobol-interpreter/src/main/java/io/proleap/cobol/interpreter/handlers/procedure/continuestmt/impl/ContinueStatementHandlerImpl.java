package io.proleap.cobol.interpreter.handlers.procedure.continuestmt.impl;

import javax.annotation.PostConstruct;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.continuestmt.ContinueStatement;
import io.proleap.cobol.interpreter.handlers.procedure.continuestmt.ContinueStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class ContinueStatementHandlerImpl extends StatementHandlerImpl<ContinueStatement>
		implements ContinueStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.CONTINUE;
	}

	@Override
	public void run(final ContinueStatement statement, final CobolInterpreterParams params) {
		// NOOP
	}
}
