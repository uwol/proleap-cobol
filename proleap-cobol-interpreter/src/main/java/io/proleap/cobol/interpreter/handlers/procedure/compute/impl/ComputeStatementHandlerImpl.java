package io.proleap.cobol.interpreter.handlers.procedure.compute.impl;

import java.math.BigDecimal;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.compute.ComputeStatement;
import io.proleap.cobol.asg.metamodel.procedure.compute.Store;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.impl.CobolDecimalValueImpl;
import io.proleap.cobol.interpreter.handlers.procedure.compute.ComputeStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class ComputeStatementHandlerImpl extends StatementHandlerImpl<ComputeStatement>
		implements ComputeStatementHandler {

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
		return StatementTypeEnum.COMPUTE;
	}

	@Override
	public void run(final ComputeStatement statement, final CobolInterpreterParams params) {
		final BigDecimal value = valueService.getAsDecimal(
				valueStmtService.getValue(statement.getArithmeticExpression(), params.getState().getStorage()));

		for (final Store store : statement.getStores()) {
			final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(store.getStoreCall());
			storageService.putValue(dataDescriptionEntry, CobolDecimalValueImpl.of(value),
					params.getState().getStorage());
		}
	}
}
