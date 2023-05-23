package io.proleap.cobol.interpreter.handlers.procedure.write.impl;

import java.math.BigDecimal;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.write.AdvancingLines;
import io.proleap.cobol.asg.metamodel.procedure.write.AdvancingPhrase;
import io.proleap.cobol.asg.metamodel.procedure.write.From;
import io.proleap.cobol.asg.metamodel.procedure.write.WriteStatement;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.write.WriteStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.file.CobolFileAccessService;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class WriteStatementHandlerImpl extends StatementHandlerImpl<WriteStatement> implements WriteStatementHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolFileAccessService fileAccessService;

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
		return StatementTypeEnum.WRITE;
	}

	@Override
	public void run(final WriteStatement statement, final CobolInterpreterParams params) {
		final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
				.getDataDescriptionEntry(statement.getRecordCall());
		final FileDescriptionEntry fileDescriptionEntry = (FileDescriptionEntry) dataDescriptionEntry
				.getDataDescriptionEntryContainer();

		final FileControlEntry fileControlEntry = fileDescriptionEntry.getFileControlEntry();
		final CobolFile file = params.getState().getFile(fileControlEntry);

		final AdvancingPhrase advancingPhrase = statement.getAdvancingPhrase();

		if (advancingPhrase != null) {
			final AdvancingLines advancingLines = advancingPhrase.getAdvancingLines();

			if (advancingLines != null) {
				final CobolValue linesValue = valueStmtService.getValue(advancingLines.getLinesValueStmt(),
						params.getState().getStorage());
				final BigDecimal linesDecimalValue = valueService.getAsDecimal(linesValue);

				for (int i = 0; i < linesDecimalValue.intValue(); i++) {
					fileAccessService.write("", file, fileControlEntry);
				}
			}
		}

		{
			final From from = statement.getFrom();
			final CobolValue value;

			if (from == null) {
				value = storageService.getValue(dataDescriptionEntry, params.getState().getStorage());
			} else {
				value = valueStmtService.getValue(from.getFromValueStmt(), params.getState().getStorage());
			}

			final ProgramUnit programUnit = statement.getProgramUnit();
			final String string = valueService.getAsString(value, programUnit);
			fileAccessService.write(string, file, fileControlEntry);
		}

		// FIXME implement invalid key phrases
		statement.getNotInvalidKeyPhrase();
		statement.getInvalidKeyPhrase();
	}
}
