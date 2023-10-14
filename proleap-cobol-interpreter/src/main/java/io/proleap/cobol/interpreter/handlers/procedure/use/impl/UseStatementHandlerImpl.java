package io.proleap.cobol.interpreter.handlers.procedure.use.impl;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.use.UseStatement;
import io.proleap.cobol.interpreter.exception.CobolInterpreterException;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.use.UseStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class UseStatementHandlerImpl extends StatementHandlerImpl<UseStatement> implements UseStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.USE;
	}

	@Override
	public void run(final UseStatement statement, final CobolInterpreterParams params) {
		// TODO declaratives: The USE statement can be used to handle program file I/O
		// errors. USE is not executed, but, rather, describes the conditions under
		// which the contained procedures are to be executed. The USE statement is
		// located in a DECLARATIVES section in a program's PROCEDURE DIVISION.
		throw new CobolInterpreterException("Missing handler for USE statement");
	}
}
