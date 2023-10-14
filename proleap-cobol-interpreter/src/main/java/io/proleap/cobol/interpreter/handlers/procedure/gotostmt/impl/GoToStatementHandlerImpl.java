package io.proleap.cobol.interpreter.handlers.procedure.gotostmt.impl;

import java.math.BigDecimal;
import java.util.List;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.ProcedureCall;
import io.proleap.cobol.asg.metamodel.call.SectionCall;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.DependingOnPhrase;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.GoToStatement;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.GoToStatement.GoToType;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.Simple;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.handlers.procedure.ParagraphHandler;
import io.proleap.cobol.interpreter.handlers.procedure.SectionHandler;
import io.proleap.cobol.interpreter.handlers.procedure.gotostmt.GoToStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class GoToStatementHandlerImpl extends StatementHandlerImpl<GoToStatement> implements GoToStatementHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolStorageService storageService;

	@Inject
	private ParagraphHandler paragraphHandler;

	@Inject
	private SectionHandler sectionHandler;

	@Inject
	private CobolValueService valueService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.GO_TO;
	}

	protected void run(final DependingOnPhrase dependingOnPhrase, final CobolInterpreterParams params) {
		final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
				.getDataDescriptionEntry(dependingOnPhrase.getDependingOnCall());
		final CobolValue dependingOnValue = storageService.getValue(dataDescriptionEntry, params.getState().getStorage());

		final BigDecimal dependingOnBigDecimal = valueService.getAsDecimal(dependingOnValue);
		final int intValue = dependingOnBigDecimal.intValue();

		final List<Call> procedureCalls = dependingOnPhrase.getProcedureCalls();
		final Call call = procedureCalls.get(intValue);
		runCall(call, params);
	}

	@Override
	public void run(final GoToStatement statement, final CobolInterpreterParams params) {
		final GoToType goToType = statement.getGoToType();

		switch (goToType) {
		case DEPENDING_ON:
			final DependingOnPhrase dependingOnPhrase = statement.getDependingOnPhrase();
			run(dependingOnPhrase, params);
			break;
		case SIMPLE:
			final Simple simple = statement.getSimple();
			run(simple, params);
			break;
		default:
		}
	}

	protected void run(final Simple simple, final CobolInterpreterParams params) {
		final Call call = simple.getProcedureCall();
		runCall(call, params);
	}

	protected void runCall(final Call call, final CobolInterpreterParams params) {
		final CallType callType = call.getCallType();

		params.getState().getStatementsHandlerScopes().haltAll();

		switch (callType) {
		case PROCEDURE_CALL:
			final ProcedureCall procedureCall = (ProcedureCall) call.unwrap();
			runProcedureCall(procedureCall, params);
			break;
		case SECTION_CALL:
			final SectionCall sectionCall = (SectionCall) call.unwrap();
			runSectionCall(sectionCall, params);
			break;
		default:
		}
	}

	protected void runProcedureCall(final ProcedureCall call, final CobolInterpreterParams params) {
		final Paragraph paragraph = call.getParagraph();
		paragraphHandler.run(paragraph, params);
	}

	protected void runSectionCall(final SectionCall call, final CobolInterpreterParams params) {
		final Section section = call.getSection();
		sectionHandler.run(section, params);
	}
}
