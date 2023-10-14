package io.proleap.cobol.interpreter.handlers.procedure.exhibit.impl;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.exhibit.ExhibitStatement;
import io.proleap.cobol.asg.metamodel.procedure.exhibit.Operand;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.handlers.procedure.exhibit.ExhibitStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class ExhibitStatementHandlerImpl extends StatementHandlerImpl<ExhibitStatement>
		implements ExhibitStatementHandler {

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
		return StatementTypeEnum.EXHIBIT;
	}

	@Override
	public void run(final ExhibitStatement statement, final CobolInterpreterParams params) {
		final StringBuffer sb = new StringBuffer();

		for (final Operand operand : statement.getOperands()) {
			final ValueStmt operandValueStmt = operand.getOperandValueStmt();
			final CobolValue value = valueStmtService.getValue(operandValueStmt, params.getState().getStorage());
			sb.append(valueService.getAsString(value, statement.getProgramUnit()));
		}

		params.getState().getConsole().addLine(sb.toString(), false);
	}
}
