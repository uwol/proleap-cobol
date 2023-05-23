package io.proleap.cobol.interpreter.handlers.procedure.close.impl;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.FileControlEntryCall;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.close.CloseFile;
import io.proleap.cobol.asg.metamodel.procedure.close.CloseStatement;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.handlers.procedure.close.CloseStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.file.CobolFileAccessService;

@Singleton
public class CloseStatementHandlerImpl extends StatementHandlerImpl<CloseStatement> implements CloseStatementHandler {

	@Inject
	private CobolFileAccessService fileAccessService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.CLOSE;
	}

	@Override
	public void run(final CloseStatement statement, final CobolInterpreterParams params) {
		for (final CloseFile closeFile : statement.getCloseFiles()) {
			final FileControlEntryCall fileCall = (FileControlEntryCall) closeFile.getFileCall().unwrap();
			final FileControlEntry fileControlEntry = fileCall.getFileControlEntry();
			final CobolFile file = params.getState().getFile(fileControlEntry);
			fileAccessService.close(file, fileControlEntry);
		}
	}
}
