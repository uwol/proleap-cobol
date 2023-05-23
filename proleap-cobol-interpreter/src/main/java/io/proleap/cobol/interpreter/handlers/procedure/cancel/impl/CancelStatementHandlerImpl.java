package io.proleap.cobol.interpreter.handlers.procedure.cancel.impl;

import javax.annotation.PostConstruct;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.cancel.CancelStatement;
import io.proleap.cobol.interpreter.handlers.procedure.cancel.CancelStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class CancelStatementHandlerImpl extends StatementHandlerImpl<CancelStatement>
		implements CancelStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.CANCEL;
	}

	@Override
	public void run(final CancelStatement statement, final CobolInterpreterParams params) {
		// FIXME storage: Implement CANCEL statement in context of CALL statement.
		// The CANCEL statement ensures that a referenced program
		// will be in an initial state the next time it is called, and to unload any
		// resources for the module.
	}
}
