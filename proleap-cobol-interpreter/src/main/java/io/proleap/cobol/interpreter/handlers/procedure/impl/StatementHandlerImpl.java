package io.proleap.cobol.interpreter.handlers.procedure.impl;

import jakarta.inject.Inject;

import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.interpreter.handlers.impl.AbstractCobolInterpreterHandler;
import io.proleap.cobol.interpreter.handlers.procedure.StatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.StatementsHandler;

public abstract class StatementHandlerImpl<S extends Statement> extends AbstractCobolInterpreterHandler
		implements StatementHandler<S> {

	@Inject
	protected StatementsHandler statementsHandler;

	@Override
	public abstract StatementType getStatementType();
}
