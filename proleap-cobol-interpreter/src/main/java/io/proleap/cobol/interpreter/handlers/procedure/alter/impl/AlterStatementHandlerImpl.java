package io.proleap.cobol.interpreter.handlers.procedure.alter.impl;

import javax.annotation.PostConstruct;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.alter.AlterStatement;
import io.proleap.cobol.interpreter.exception.CobolInterpreterException;
import io.proleap.cobol.interpreter.handlers.procedure.alter.AlterStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class AlterStatementHandlerImpl extends StatementHandlerImpl<AlterStatement> implements AlterStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.ALTER;
	}

	@Override
	public void run(final AlterStatement statement, final CobolInterpreterParams params) {
		// FIXME control flow: The ALTER statement changes the transfer point specified
		// in a GO TO statement.
		throw new CobolInterpreterException("Missing handler for ALTER statement");
	}
}
