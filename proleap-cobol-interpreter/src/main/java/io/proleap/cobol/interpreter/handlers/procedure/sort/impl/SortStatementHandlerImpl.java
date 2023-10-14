package io.proleap.cobol.interpreter.handlers.procedure.sort.impl;

import java.util.List;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.FileControlEntryCall;
import io.proleap.cobol.asg.metamodel.call.ProcedureCall;
import io.proleap.cobol.asg.metamodel.call.SectionCall;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.sort.Giving;
import io.proleap.cobol.asg.metamodel.procedure.sort.GivingPhrase;
import io.proleap.cobol.asg.metamodel.procedure.sort.InputProcedure;
import io.proleap.cobol.asg.metamodel.procedure.sort.OutputProcedure;
import io.proleap.cobol.asg.metamodel.procedure.sort.SortStatement;
import io.proleap.cobol.asg.metamodel.procedure.sort.UsingPhrase;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.handlers.procedure.ParagraphHandler;
import io.proleap.cobol.interpreter.handlers.procedure.SectionHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.sort.SortStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.address.CobolFileControlEntryAddressService;
import io.proleap.cobol.interpreter.service.file.CobolFileAccessService;
import io.proleap.cobol.interpreter.service.file.criteria.CobolFileCriteriaService;
import io.proleap.cobol.interpreter.service.file.domain.SortCriterion;

@Singleton
public class SortStatementHandlerImpl extends StatementHandlerImpl<SortStatement> implements SortStatementHandler {

	@Inject
	private CobolFileAccessService fileAccessService;

	@Inject
	private CobolFileControlEntryAddressService fileControlEntryAddressService;

	@Inject
	private CobolFileCriteriaService fileCriteriaService;

	@Inject
	private ParagraphHandler paragraphHandler;

	@Inject
	private SectionHandler sectionHandler;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.SORT;
	}

	@Override
	public void run(final SortStatement statement, final CobolInterpreterParams params) {
		final InputProcedure inputProcedure = statement.getInputProcedure();

		final Call fileCallToSort = statement.getFileCall();
		final FileControlEntryCall fileControlEntryCallToSort = (FileControlEntryCall) fileCallToSort.unwrap();
		final FileControlEntry fileControlEntryToSort = fileControlEntryCallToSort.getFileControlEntry();
		final CobolFile entityToSort = fileAccessService.assureFile(fileControlEntryToSort, params.getState());

		if (inputProcedure != null) {
			for (final Call procedureCall : inputProcedure.getCalls()) {
				runCall(procedureCall, params);
			}
		}

		for (final UsingPhrase usingPhrase : statement.getUsingPhrases()) {
			for (final Call fileCallToCopy : usingPhrase.getFileCalls()) {
				final FileControlEntryCall fileControlEntryCallToCopy = (FileControlEntryCall) fileCallToCopy.unwrap();
				final FileControlEntry fileControlEntryToCopy = fileControlEntryCallToCopy.getFileControlEntry();
				final CobolFile entityToCopy = fileAccessService.assureFile(fileControlEntryToCopy, params.getState());
				fileAccessService.copy(entityToCopy, entityToSort, fileControlEntryToSort);
			}
		}

		final List<CobolAddress> fileControlEntryAddresses = fileControlEntryAddressService
				.getAddresses(fileControlEntryToSort, params.getState().getStorage());
		final List<SortCriterion> sortCriteria = fileCriteriaService.createSortCriteria(fileControlEntryAddresses,
				statement.getOnKeys(), params);
		fileAccessService.sort(entityToSort, fileControlEntryAddresses, sortCriteria, fileControlEntryToSort,
				statement.getProgramUnit());

		for (final GivingPhrase givingPhrase : statement.getGivingPhrases()) {
			runGiving(givingPhrase, entityToSort, params);
		}

		final OutputProcedure outputProcedure = statement.getOutputProcedure();

		if (outputProcedure != null) {
			for (final Call procedureCall : outputProcedure.getCalls()) {
				runCall(procedureCall, params);
			}
		}
	}

	protected void runCall(final Call call, final CobolInterpreterParams params) {
		final CallType callType = call.getCallType();

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

	protected void runGiving(final GivingPhrase givingPhrase, final CobolFile file,
			final CobolInterpreterParams params) {
		for (final Giving giving : givingPhrase.getGivings()) {
			final Call givingFileCall = giving.getFileCall();
			final FileControlEntryCall givingFileControlEntryCall = (FileControlEntryCall) givingFileCall.unwrap();

			if (givingFileControlEntryCall != null) {
				final FileControlEntry fileControlEntry = givingFileControlEntryCall.getFileControlEntry();

				if (fileControlEntry != null) {
					final CobolFile givingEntity = fileAccessService.assureFile(fileControlEntry, params.getState());
					fileAccessService.copy(file, givingEntity, fileControlEntry);
				}
			}
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
