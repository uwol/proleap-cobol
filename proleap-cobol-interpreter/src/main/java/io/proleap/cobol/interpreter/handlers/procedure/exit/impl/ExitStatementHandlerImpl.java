package io.proleap.cobol.interpreter.handlers.procedure.exit.impl;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.exit.ExitStatement;
import io.proleap.cobol.interpreter.domain.scope.CobolInterpreterScope;
import io.proleap.cobol.interpreter.domain.scope.CobolInterpreterScopeStack;
import io.proleap.cobol.interpreter.handlers.procedure.exit.ExitStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class ExitStatementHandlerImpl extends StatementHandlerImpl<ExitStatement> implements ExitStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.EXIT;
	}

	@Override
	public void run(final ExitStatement statement, final CobolInterpreterParams params) {
		final CobolInterpreterScopeStack scopeStack = params.getState().getPerformScopes();

		if (!scopeStack.getScopes().empty()) {
			final CobolInterpreterScope scope = scopeStack.peek();
			scope.setHalted(true);
		}
	}
}
