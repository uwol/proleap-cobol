package io.proleap.cobol.interpreter.handlers.procedure.merge.impl;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.FileControlEntryCall;
import io.proleap.cobol.asg.metamodel.call.ProcedureCall;
import io.proleap.cobol.asg.metamodel.call.SectionCall;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.merge.Giving;
import io.proleap.cobol.asg.metamodel.procedure.merge.GivingPhrase;
import io.proleap.cobol.asg.metamodel.procedure.merge.MergeStatement;
import io.proleap.cobol.asg.metamodel.procedure.merge.OnKey;
import io.proleap.cobol.asg.metamodel.procedure.merge.OnKey.OnKeyType;
import io.proleap.cobol.asg.metamodel.procedure.merge.OutputProcedurePhrase;
import io.proleap.cobol.asg.metamodel.procedure.merge.UsingPhrase;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.handlers.procedure.ParagraphHandler;
import io.proleap.cobol.interpreter.handlers.procedure.SectionHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.merge.MergeStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.address.CobolFileControlEntryAddressService;
import io.proleap.cobol.interpreter.service.file.CobolFileAccessService;
import io.proleap.cobol.interpreter.service.file.domain.SortCriterion;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class MergeStatementHandlerImpl extends StatementHandlerImpl<MergeStatement> implements MergeStatementHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolFileAccessService fileAccessService;

	@Inject
	private CobolFileControlEntryAddressService fileControlEntryAddressService;

	@Inject
	private ParagraphHandler paragraphHandler;

	@Inject
	private SectionHandler sectionHandler;

	@Inject
	private CobolStorageService storageService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	private List<SortCriterion> createSortCriteria(final List<CobolAddress> fileControlEntryAddresses,
			final MergeStatement statement, final CobolInterpreterParams params) {
		final List<SortCriterion> result = new ArrayList<>();

		for (final OnKey onKey : statement.getOnKeys()) {
			for (final Call call : onKey.getKeyCalls()) {
				final DataDescriptionEntry onEntry = dataDescriptionEntryService.getDataDescriptionEntry(call);
				final CobolAddressGroup addressGroup = storageService.getAddressGroup(onEntry,
						params.getState().getStorage());

				final SortCriterion sortCriterion = new SortCriterion();
				sortCriterion.descending = OnKeyType.DESCENDING.equals(onKey.getOnKeyType());

				for (final CobolAddress address : addressGroup.getAddresses()) {
					final int position = fileControlEntryAddresses.indexOf(address);
					sortCriterion.positions.add(position);
				}

				result.add(sortCriterion);
			}
		}

		return result;
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.MERGE;
	}

	@Override
	public void run(final MergeStatement statement, final CobolInterpreterParams params) {
		final Call fileCallToSort = statement.getFileCall();
		final FileControlEntryCall fileControlEntryCallToSort = (FileControlEntryCall) fileCallToSort.unwrap();
		final FileControlEntry fileControlEntryToSort = fileControlEntryCallToSort.getFileControlEntry();
		final CobolFile entityToSort = fileAccessService.assureFile(fileControlEntryToSort, params.getState());

		for (final UsingPhrase usingPhrase : statement.getUsingPhrases()) {
			for (final Call fileCallToCopy : usingPhrase.getFileCalls()) {
				final FileControlEntryCall fileControlEntryCallToCopy = (FileControlEntryCall) fileCallToCopy.unwrap();
				final FileControlEntry fileControlEntryToCopy = fileControlEntryCallToCopy.getFileControlEntry();
				final CobolFile entityToCopy = fileAccessService.assureFile(fileControlEntryToCopy, params.getState());
				fileAccessService.copy(entityToCopy, entityToSort, fileControlEntryToSort);
			}
		}

		final List<CobolAddress> addressesToSort = fileControlEntryAddressService.getAddresses(fileControlEntryToSort,
				params.getState().getStorage());
		final List<SortCriterion> sortCriteria = createSortCriteria(addressesToSort, statement, params);
		fileAccessService.sort(entityToSort, addressesToSort, sortCriteria, fileControlEntryToSort,
				statement.getProgramUnit());

		final OutputProcedurePhrase outputProcedurePhrase = statement.getOutputProcedurePhrase();

		for (final GivingPhrase givingPhrase : statement.getGivingPhrases()) {
			runGiving(givingPhrase, entityToSort, params);
		}

		if (outputProcedurePhrase != null) {
			for (final Call procedureCall : outputProcedurePhrase.getCalls()) {
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
			final FileControlEntryCall fileControlEntryCall = (FileControlEntryCall) givingFileCall.unwrap();

			if (fileControlEntryCall != null) {
				final FileControlEntry fileControlEntry = fileControlEntryCall.getFileControlEntry();

				if (fileControlEntry != null) {
					final CobolFile givingEntity = fileAccessService.assureFile(fileControlEntry,
							params.getState());
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
