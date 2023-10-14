package io.proleap.cobol.interpreter.handlers.procedure.delete.impl;

import java.util.List;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.FileControlEntryCall;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.AlternateRecordKeyClause;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.RecordKeyClause;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.delete.DeleteStatement;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.handlers.procedure.delete.DeleteStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.address.CobolFileControlEntryAddressService;
import io.proleap.cobol.interpreter.service.file.CobolFileAccessService;
import io.proleap.cobol.interpreter.service.file.criteria.CobolFileCriteriaService;
import io.proleap.cobol.interpreter.service.file.domain.KeyCriterion;

@Singleton
public class DeleteStatementHandlerImpl extends StatementHandlerImpl<DeleteStatement>
		implements DeleteStatementHandler {

	@Inject
	private CobolFileAccessService fileAccessService;

	@Inject
	private CobolFileControlEntryAddressService fileControlEntryAddressService;

	@Inject
	private CobolFileCriteriaService fileCriteriaService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.DELETE;
	}

	@Override
	public void run(final DeleteStatement statement, final CobolInterpreterParams params) {
		final Call call = statement.getFileCall();
		final FileControlEntryCall fileControlEntryCall = (FileControlEntryCall) call.unwrap();
		final FileControlEntry fileControlEntry = fileControlEntryCall.getFileControlEntry();

		final CobolFile entity = params.getState().getFile(fileControlEntry);
		final List<CobolAddress> fileControlEntryAddresses = fileControlEntryAddressService
				.getAddresses(fileControlEntry, params.getState().getStorage());

		final RecordKeyClause recordKeyClause = fileControlEntry.getRecordKeyClause();
		final AlternateRecordKeyClause alternateRecordKeyClause = fileControlEntry.getAlternateRecordKeyClause();
		final KeyCriterion keyCriterion;

		if (recordKeyClause != null) {
			keyCriterion = fileCriteriaService.createKeyCriterion(recordKeyClause, fileControlEntryAddresses, params);
		} else if (alternateRecordKeyClause != null) {
			keyCriterion = fileCriteriaService.createKeyCriterion(alternateRecordKeyClause, fileControlEntryAddresses,
					params);
		} else {
			keyCriterion = null;
		}

		fileAccessService.delete(entity, fileControlEntryAddresses, keyCriterion, fileControlEntry,
				statement.getProgramUnit());

		// FIXME implement invalid key phrases
		statement.getNotInvalidKeyPhrase();
		statement.getInvalidKeyPhrase();
	}
}
