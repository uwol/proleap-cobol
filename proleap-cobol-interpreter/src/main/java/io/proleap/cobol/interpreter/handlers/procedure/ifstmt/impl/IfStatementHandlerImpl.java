package io.proleap.cobol.interpreter.handlers.procedure.ifstmt.impl;

import java.util.List;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.Else;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.IfStatement;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.Then;
import io.proleap.cobol.asg.metamodel.valuestmt.ConditionValueStmt;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.interpreter.handlers.procedure.StatementsHandler;
import io.proleap.cobol.interpreter.handlers.procedure.ifstmt.IfStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class IfStatementHandlerImpl extends StatementHandlerImpl<IfStatement> implements IfStatementHandler {

	@Inject
	private StatementsHandler statementsHandler;

	@Inject
	private CobolValueService valueService;

	@Inject
	private CobolValueStmtService valueStmtService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.IF;
	}

	@Override
	public void run(final IfStatement statement, final CobolInterpreterParams params) {
		final ConditionValueStmt condition = statement.getCondition();
		final Boolean conditionValue = valueService
				.getBoolean(valueStmtService.getValue(condition, params.getState().getStorage()));

		if (Boolean.TRUE.equals(conditionValue)) {
			final Then then = statement.getThen();

			if (then != null) {
				final List<Statement> statements = then.getStatements();
				statementsHandler.run(statements, params);
			} else {
			}
		} else {
			final Else elsePhrase = statement.getElse();

			if (elsePhrase != null) {
				final List<Statement> statements = elsePhrase.getStatements();
				statementsHandler.run(statements, params);
			} else {
			}
		}
	}
}
