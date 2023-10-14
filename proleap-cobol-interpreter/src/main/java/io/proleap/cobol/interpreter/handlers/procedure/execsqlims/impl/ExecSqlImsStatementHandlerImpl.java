package io.proleap.cobol.interpreter.handlers.procedure.execsqlims.impl;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.execsqlims.ExecSqlImsStatement;
import io.proleap.cobol.interpreter.handlers.procedure.execsqlims.ExecSqlImsStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class ExecSqlImsStatementHandlerImpl extends StatementHandlerImpl<ExecSqlImsStatement>
		implements ExecSqlImsStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.EXEC_SQLIMS;
	}

	@Override
	public void run(final ExecSqlImsStatement statement, final CobolInterpreterParams params) {
		// TODO integrate SQL IMS
	}
}
