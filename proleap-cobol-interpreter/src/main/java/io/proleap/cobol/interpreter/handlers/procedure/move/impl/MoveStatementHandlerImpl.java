package io.proleap.cobol.interpreter.handlers.procedure.move.impl;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveCorrespondingToSendingArea;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveCorrespondingToStatetement;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveStatement;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveStatement.MoveType;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveToSendingArea;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveToStatement;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.move.MoveStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class MoveStatementHandlerImpl extends StatementHandlerImpl<MoveStatement> implements MoveStatementHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolStorageService storageService;

	@Inject
	private CobolValueStmtService valueStmtService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.MOVE;
	}

	protected void run(final MoveCorrespondingToStatetement moveCorrespondingToStatement,
			final CobolInterpreterParams params) {
		final MoveCorrespondingToSendingArea sendingArea = moveCorrespondingToStatement
				.getMoveToCorrespondingSendingArea();
		final DataDescriptionEntryGroup sendingGroup = (DataDescriptionEntryGroup) dataDescriptionEntryService
				.getDataDescriptionEntry(sendingArea.getSendingAreaCall());

		for (final Call receivingGroupCall : moveCorrespondingToStatement.getReceivingAreaCalls()) {
			final DataDescriptionEntryGroup receivingGroup = (DataDescriptionEntryGroup) dataDescriptionEntryService
					.getDataDescriptionEntry(receivingGroupCall);

			for (final DataDescriptionEntry sendingChild : sendingGroup.getDataDescriptionEntries()) {
				final String name = sendingChild.getName();
				final DataDescriptionEntry receivingChild = receivingGroup.getDataDescriptionEntry(name);

				final CobolValue sendingValue = storageService.getValue(sendingChild, params.getState().getStorage());
				storageService.putValue(receivingChild, sendingValue, params.getState().getStorage());
			}
		}
	}

	@Override
	public void run(final MoveStatement statement, final CobolInterpreterParams params) {
		final MoveType moveType = statement.getMoveType();

		switch (moveType) {
		case MOVE_CORRESPONDING:
			run(statement.getMoveCorrespondingToStatement(), params);
			break;
		case MOVE_TO:
			run(statement.getMoveToStatement(), params);
			break;
		}
	}

	protected void run(final MoveToStatement moveToStatement, final CobolInterpreterParams params) {
		final MoveToSendingArea sendingArea = moveToStatement.getSendingArea();
		final CobolValue value = valueStmtService.getValue(sendingArea.getSendingAreaValueStmt(),
				params.getState().getStorage());

		for (final Call call : moveToStatement.getReceivingAreaCalls()) {
			final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService.getDataDescriptionEntry(call);
			storageService.putValue(dataDescriptionEntry, value, params.getState().getStorage());
		}
	}
}
