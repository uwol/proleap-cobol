package io.proleap.cobol.interpreter.handlers.procedure.initiate.impl;

import javax.annotation.PostConstruct;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.initiate.InitiateStatement;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.initiate.InitiateStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class InitiateStatementHandlerImpl extends StatementHandlerImpl<InitiateStatement>
		implements InitiateStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.INITIATE;
	}

	@Override
	public void run(final InitiateStatement statement, final CobolInterpreterParams params) {
	}
}
