package io.proleap.cobol.interpreter.handlers.procedure.rewrite.impl;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.rewrite.From;
import io.proleap.cobol.asg.metamodel.procedure.rewrite.RewriteStatement;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.rewrite.RewriteStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.file.CobolFileAccessService;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class RewriteStatementHandlerImpl extends StatementHandlerImpl<RewriteStatement>
		implements RewriteStatementHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolFileAccessService fileAccessService;

	@Inject
	private CobolStorageService storageService;

	@Inject
	private CobolValueService valueService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.REWRITE;
	}

	@Override
	public void run(final RewriteStatement statement, final CobolInterpreterParams params) {
		final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
				.getDataDescriptionEntry(statement.getRecordCall());
		final FileDescriptionEntry fileDescriptionEntry = (FileDescriptionEntry) dataDescriptionEntry
				.getDataDescriptionEntryContainer();

		final FileControlEntry fileControlEntry = fileDescriptionEntry.getFileControlEntry();
		final CobolFile file = params.getState().getFile(fileControlEntry);

		final From from = statement.getFrom();
		final CobolValue value;

		if (from == null) {
			value = storageService.getValue(dataDescriptionEntry, params.getState().getStorage());
		} else {
			final DataDescriptionEntry fromDataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(from.getFromCall());
			value = storageService.getValue(fromDataDescriptionEntry, params.getState().getStorage());
		}

		final String record = valueService.getAsString(value, statement.getProgramUnit());
		fileAccessService.rewrite(record, file, fileControlEntry);

		// FIXME implement invalid key phrases
		statement.getNotInvalidKeyPhrase();
		statement.getInvalidKeyPhrase();
	}
}
