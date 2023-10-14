package io.proleap.cobol.interpreter.handlers.procedure.execsql.impl;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.execsql.ExecSqlStatement;
import io.proleap.cobol.interpreter.handlers.procedure.execsql.ExecSqlStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class ExecSqlStatementHandlerImpl extends StatementHandlerImpl<ExecSqlStatement>
		implements ExecSqlStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.EXEC_SQL;
	}

	@Override
	public void run(final ExecSqlStatement statement, final CobolInterpreterParams params) {
		// TODO integrate SQL
	}
}
