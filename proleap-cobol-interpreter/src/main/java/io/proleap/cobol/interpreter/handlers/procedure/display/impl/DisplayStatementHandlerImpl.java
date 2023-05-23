package io.proleap.cobol.interpreter.handlers.procedure.display.impl;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.display.DisplayStatement;
import io.proleap.cobol.asg.metamodel.procedure.display.Operand;
import io.proleap.cobol.asg.metamodel.procedure.display.Upon;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.handlers.procedure.display.DisplayStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class DisplayStatementHandlerImpl extends StatementHandlerImpl<DisplayStatement>
		implements DisplayStatementHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolStorageService storageService;

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
		return StatementTypeEnum.DISPLAY;
	}

	@Override
	public void run(final DisplayStatement statement, final CobolInterpreterParams params) {
		final Upon upon = statement.getUpon();
		final boolean condition;

		if (upon == null) {
			condition = true;
		} else {
			final Call uponCall = upon.getUponCall();
			final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(uponCall);
			final CobolValue uponValue = storageService.getValue(dataDescriptionEntry, params.getState().getStorage());
			final Boolean uponBoolean = valueService.getBoolean(uponValue);
			condition = Boolean.TRUE.equals(uponBoolean);
		}

		if (condition) {
			final StringBuffer sb = new StringBuffer();

			for (final Operand operand : statement.getOperands()) {
				final ValueStmt operandValueStmt = operand.getOperandValueStmt();
				final CobolValue value = valueStmtService.getValue(operandValueStmt, params.getState().getStorage());
				sb.append(valueService.getAsString(value, statement.getProgramUnit()));
			}

			final boolean noAdvancing = statement.getWith() != null && statement.getWith().isNoAdvancing();
			params.getState().getConsole().addLine(sb.toString(), noAdvancing);
		}
	}
}
