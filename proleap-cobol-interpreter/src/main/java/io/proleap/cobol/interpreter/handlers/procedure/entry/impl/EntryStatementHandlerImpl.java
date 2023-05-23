package io.proleap.cobol.interpreter.handlers.procedure.entry.impl;

import javax.annotation.PostConstruct;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.entry.EntryStatement;
import io.proleap.cobol.interpreter.exception.CobolInterpreterException;
import io.proleap.cobol.interpreter.handlers.procedure.entry.EntryStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class EntryStatementHandlerImpl extends StatementHandlerImpl<EntryStatement> implements EntryStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.ENTRY;
	}

	@Override
	public void run(final EntryStatement statement, final CobolInterpreterParams params) {
		// FIXME control flow: The ENTRY statement establishes an alternate entry point
		// into a COBOL called subprogram. When a CALL statement that specifies the
		// alternate entry point is executed in a calling program, control is
		// transferred to the next executable statement following the ENTRY statement.
		throw new CobolInterpreterException("Missing handler for ENTRY statement");
	}
}
