package io.proleap.cobol.interpreter.handlers.procedure.open.impl;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.FileControlEntryCall;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.open.ExtendPhrase;
import io.proleap.cobol.asg.metamodel.procedure.open.Input;
import io.proleap.cobol.asg.metamodel.procedure.open.InputOutputPhrase;
import io.proleap.cobol.asg.metamodel.procedure.open.InputPhrase;
import io.proleap.cobol.asg.metamodel.procedure.open.OpenStatement;
import io.proleap.cobol.asg.metamodel.procedure.open.Output;
import io.proleap.cobol.asg.metamodel.procedure.open.OutputPhrase;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.domain.file.impl.CobolSequentialFileImpl;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.open.OpenStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.file.CobolFileAccessService;

@Singleton
public class OpenStatementHandlerImpl extends StatementHandlerImpl<OpenStatement> implements OpenStatementHandler {

	@Inject
	private CobolFileAccessService fileAccessService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.OPEN;
	}

	protected void processCall(final Call call, final CobolInterpreterParams params) {
		if (CallType.FILE_CONTROL_ENTRY_CALL.equals(call.getCallType())) {
			final FileControlEntryCall fileControlEntryCall = (FileControlEntryCall) call.unwrap();
			final FileControlEntry fileControlEntry = fileControlEntryCall.getFileControlEntry();

			if (params.getState().getFile(fileControlEntry) == null) {
				params.getState().putFile(fileControlEntry, new CobolSequentialFileImpl());
			}

			final CobolFile file = params.getState().getFile(fileControlEntry);
			fileAccessService.open(file, fileControlEntry);
		}
	}

	@Override
	public void run(final OpenStatement statement, final CobolInterpreterParams params) {
		for (final InputOutputPhrase inputOutputPhrase : statement.getInputOutputPhrases()) {
			for (final Call call : inputOutputPhrase.getFileCalls()) {
				processCall(call, params);
			}
		}

		for (final ExtendPhrase extendPhrase : statement.getExtendPhrases()) {
			for (final Call call : extendPhrase.getFileCalls()) {
				processCall(call, params);
			}
		}

		for (final InputPhrase inputPhrase : statement.getInputPhrases()) {
			for (final Input input : inputPhrase.getInputs()) {
				final Call call = input.getFileCall();
				processCall(call, params);
			}
		}

		for (final OutputPhrase outputPhrase : statement.getOutputPhrases()) {
			for (final Output output : outputPhrase.getOutputs()) {
				final Call call = output.getFileCall();
				processCall(call, params);
			}
		}
	}
}
