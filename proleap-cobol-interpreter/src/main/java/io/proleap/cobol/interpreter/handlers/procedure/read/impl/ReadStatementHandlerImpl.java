package io.proleap.cobol.interpreter.handlers.procedure.read.impl;

import java.util.List;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.FileControlEntryCall;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.AlternateRecordKeyClause;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.RecordKeyClause;
import io.proleap.cobol.asg.metamodel.procedure.AtEndPhrase;
import io.proleap.cobol.asg.metamodel.procedure.NotAtEndPhrase;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.read.Into;
import io.proleap.cobol.asg.metamodel.procedure.read.Key;
import io.proleap.cobol.asg.metamodel.procedure.read.ReadStatement;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.domain.impl.CobolStringValueImpl;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.handlers.procedure.StatementsHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.read.ReadStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.address.CobolFileControlEntryAddressService;
import io.proleap.cobol.interpreter.service.file.CobolFileAccessService;
import io.proleap.cobol.interpreter.service.file.criteria.CobolFileCriteriaService;
import io.proleap.cobol.interpreter.service.file.domain.KeyCriterion;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class ReadStatementHandlerImpl extends StatementHandlerImpl<ReadStatement> implements ReadStatementHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolFileAccessService fileAccessService;

	@Inject
	private CobolFileControlEntryAddressService fileControlEntryAddressService;

	@Inject
	private CobolFileCriteriaService fileCriteriaService;

	@Inject
	private StatementsHandler statementsHandler;

	@Inject
	private CobolStorageService storageService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.READ;
	}

	@Override
	public void run(final ReadStatement statement, final CobolInterpreterParams params) {
		final Call call = statement.getFileCall();
		final FileControlEntryCall fileControlEntryCall = (FileControlEntryCall) call.unwrap();
		final FileControlEntry fileControlEntry = fileControlEntryCall.getFileControlEntry();

		final CobolFile file = params.getState().getFile(fileControlEntry);
		final List<CobolAddress> fileControlEntryAddresses = fileControlEntryAddressService
				.getAddresses(fileControlEntry, params.getState().getStorage());

		final Key key = statement.getKey();
		final RecordKeyClause recordKeyClause = fileControlEntry.getRecordKeyClause();
		final AlternateRecordKeyClause alternateRecordKeyClause = fileControlEntry.getAlternateRecordKeyClause();
		final KeyCriterion keyCriterion;

		if (key != null) {
			keyCriterion = fileCriteriaService.createKeyCriterion(key, fileControlEntryAddresses, params);
		} else if (recordKeyClause != null) {
			keyCriterion = fileCriteriaService.createKeyCriterion(recordKeyClause, fileControlEntryAddresses, params);
		} else if (alternateRecordKeyClause != null) {
			keyCriterion = fileCriteriaService.createKeyCriterion(alternateRecordKeyClause, fileControlEntryAddresses,
					params);
		} else {
			keyCriterion = null;
		}

		final String record = fileAccessService.read(file, fileControlEntryAddresses, keyCriterion, fileControlEntry,
				statement.getProgramUnit());

		if (record != null) {
			final Into into = statement.getInto();

			if (into == null) {
				final List<DataDescriptionEntry> dataDescriptionEntries = fileControlEntryAddressService
						.getDataDescriptionEntries(fileControlEntry);
				storageService.putValue(dataDescriptionEntries, CobolStringValueImpl.of(record),
						params.getState().getStorage());
			} else {
				final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
						.getDataDescriptionEntry(into.getIntoCall());
				storageService.putValue(dataDescriptionEntry, CobolStringValueImpl.of(record),
						params.getState().getStorage());
			}

			final NotAtEndPhrase notAtEndPhrase = statement.getNotAtEndPhrase();

			if (notAtEndPhrase != null) {
				statementsHandler.run(notAtEndPhrase.getStatements(), params);
			}
		} else {
			final AtEndPhrase atEndPhrase = statement.getAtEnd();

			if (atEndPhrase != null) {
				statementsHandler.run(atEndPhrase.getStatements(), params);
			}
		}

		// FIXME implement invalid key phrases
		statement.getNotInvalidKeyPhrase();
		statement.getInvalidKeyPhrase();
	}
}
