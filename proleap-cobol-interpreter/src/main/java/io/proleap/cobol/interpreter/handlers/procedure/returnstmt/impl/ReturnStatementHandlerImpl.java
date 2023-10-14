package io.proleap.cobol.interpreter.handlers.procedure.returnstmt.impl;

import java.util.List;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.FileControlEntryCall;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.procedure.AtEndPhrase;
import io.proleap.cobol.asg.metamodel.procedure.NotAtEndPhrase;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.returnstmt.Into;
import io.proleap.cobol.asg.metamodel.procedure.returnstmt.ReturnStatement;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.domain.impl.CobolStringValueImpl;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.handlers.procedure.StatementsHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.returnstmt.ReturnStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.address.CobolFileControlEntryAddressService;
import io.proleap.cobol.interpreter.service.file.CobolFileAccessService;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class ReturnStatementHandlerImpl extends StatementHandlerImpl<ReturnStatement>
		implements ReturnStatementHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolFileAccessService fileAccessService;

	@Inject
	private CobolFileControlEntryAddressService fileControlEntryAddressService;

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
		return StatementTypeEnum.RETURN;
	}

	@Override
	public void run(final ReturnStatement statement, final CobolInterpreterParams params) {
		final Call fileCall = statement.getFileCall();
		final FileControlEntryCall fileControlEntryCall = (FileControlEntryCall) fileCall.unwrap();
		final FileControlEntry fileControlEntry = fileControlEntryCall.getFileControlEntry();

		final CobolFile file = params.getState().getFile(fileControlEntry);
		final List<CobolAddress> fileControlEntryAddresses = fileControlEntryAddressService
				.getAddresses(fileControlEntry, params.getState().getStorage());
		final String record = fileAccessService.read(file, fileControlEntryAddresses, null, fileControlEntry,
				statement.getProgramUnit());

		if (record != null) {
			final Into into = statement.getInto();

			if (into == null) {
				final List<DataDescriptionEntry> dataDescriptionEntries = fileControlEntryAddressService
						.getDataDescriptionEntries(fileControlEntry);
				storageService.putValue(dataDescriptionEntries, CobolStringValueImpl.of(record),
						params.getState().getStorage());
			} else {
				final DataDescriptionEntry intoDataDescriptionEntry = dataDescriptionEntryService
						.getDataDescriptionEntry(into.getIntoCall());
				storageService.putValue(intoDataDescriptionEntry, CobolStringValueImpl.of(record),
						params.getState().getStorage());
			}

			final NotAtEndPhrase notAtEndPhrase = statement.getNotAtEndPhrase();

			if (notAtEndPhrase != null) {
				statementsHandler.run(notAtEndPhrase.getStatements(), params);
			}
		} else {
			final AtEndPhrase atEndPhrase = statement.getAtEndPhrase();

			if (atEndPhrase != null) {
				statementsHandler.run(atEndPhrase.getStatements(), params);
			}
		}
	}
}
