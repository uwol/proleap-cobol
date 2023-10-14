package io.proleap.cobol.interpreter.handlers.procedure.release.impl;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.release.ReleaseStatement;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.release.ReleaseStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.file.CobolFileAccessService;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class ReleaseStatementHandlerImpl extends StatementHandlerImpl<ReleaseStatement>
		implements ReleaseStatementHandler {

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
		return StatementTypeEnum.RELEASE;
	}

	@Override
	public void run(final ReleaseStatement statement, final CobolInterpreterParams params) {
		final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
				.getDataDescriptionEntry(statement.getRecordCall());
		final FileDescriptionEntry fileDescriptionEntry = (FileDescriptionEntry) dataDescriptionEntry
				.getDataDescriptionEntryContainer();

		final FileControlEntry fileControlEntry = fileDescriptionEntry.getFileControlEntry();
		final CobolFile file = params.getState().getFile(fileControlEntry);

		final CobolValue cobolValue = storageService.getValue(dataDescriptionEntry, params.getState().getStorage());
		final ProgramUnit programUnit = statement.getProgramUnit();
		final String string = valueService.getAsString(cobolValue, programUnit);

		fileAccessService.write(string, file, fileControlEntry);
	}
}
