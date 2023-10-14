package io.proleap.cobol.interpreter.handlers.procedure.initialize.impl;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.initialize.InitializeStatement;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.domain.impl.CobolStringValueImpl;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.initialize.InitializeStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class InitializeStatementHandlerImpl extends StatementHandlerImpl<InitializeStatement>
		implements InitializeStatementHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolStorageService storageService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.INITIALIZE;
	}

	protected void run(final DataDescriptionEntry dataDescriptionEntry, final CobolInterpreterParams params) {
		storageService.putValue(dataDescriptionEntry, new CobolStringValueImpl(), params.getState().getStorage());
	}

	@Override
	public void run(final InitializeStatement statement, final CobolInterpreterParams params) {
		for (final Call call : statement.getDataItemCalls()) {
			final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService.getDataDescriptionEntry(call);
			run(dataDescriptionEntry, params);
		}
	}
}
