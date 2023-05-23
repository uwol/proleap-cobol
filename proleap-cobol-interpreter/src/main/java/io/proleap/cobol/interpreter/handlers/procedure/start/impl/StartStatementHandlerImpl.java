package io.proleap.cobol.interpreter.handlers.procedure.start.impl;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.FileControlEntryCall;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.start.StartStatement;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.start.StartStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.file.CobolFileAccessService;

@Singleton
public class StartStatementHandlerImpl extends StatementHandlerImpl<StartStatement> implements StartStatementHandler {

	@Inject
	private CobolFileAccessService fileAccessService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.START;
	}

	@Override
	public void run(final StartStatement statement, final CobolInterpreterParams params) {
		final Call call = statement.getFileCall();
		final FileControlEntryCall fileControlEntryCall = (FileControlEntryCall) call.unwrap();
		final FileControlEntry fileControlEntry = fileControlEntryCall.getFileControlEntry();

		final CobolFile file = params.getState().getFile(fileControlEntry);
		fileAccessService.start(file, fileControlEntry);

		// FIXME implement invalid key phrases
		statement.getNotInvalidKeyPhrase();
		statement.getInvalidKeyPhrase();
	}
}
