package io.proleap.cobol.interpreter.handlers.procedure.impl;

import java.util.Iterator;
import java.util.List;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.ByReference;
import io.proleap.cobol.asg.metamodel.procedure.ByReferencePhrase;
import io.proleap.cobol.asg.metamodel.procedure.ByValue;
import io.proleap.cobol.asg.metamodel.procedure.ByValuePhrase;
import io.proleap.cobol.asg.metamodel.procedure.GivingClause;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.UsingClause;
import io.proleap.cobol.asg.metamodel.procedure.UsingParameter;
import io.proleap.cobol.asg.metamodel.procedure.UsingParameter.UsingParameterType;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;
import io.proleap.cobol.interpreter.domain.scope.CobolInterpreterScope;
import io.proleap.cobol.interpreter.handlers.impl.AbstractCobolInterpreterHandler;
import io.proleap.cobol.interpreter.handlers.procedure.ParagraphHandler;
import io.proleap.cobol.interpreter.handlers.procedure.ProcedureDivisionHandler;
import io.proleap.cobol.interpreter.handlers.procedure.SectionHandler;
import io.proleap.cobol.interpreter.handlers.procedure.StatementsHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class ProcedureDivisionHandlerImpl extends AbstractCobolInterpreterHandler implements ProcedureDivisionHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolStorageService storageService;

	@Inject
	private ParagraphHandler paragraphHandler;

	@Inject
	private SectionHandler sectionHandler;

	@Inject
	private StatementsHandler statementsHandler;

	private void initGivingParameter(final GivingClause givingClause, final CobolInterpreterParams params) {
		if (params.getGivingParam() != null) {
			final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(givingClause.getGivingCall());
			final CobolAddressGroup addressGroup = params.getGivingParam();

			storageService.putAddressGroup(dataDescriptionEntry, addressGroup, params.getState().getStorage());
		}
	}

	private void initReferenceParameters(final ByReferencePhrase byReferencePhrase,
			final CobolInterpreterParams params) {
		if (byReferencePhrase.getByReferences() != null && params.getReferenceParams() != null) {
			final int paramsSize = params.getReferenceParams().size();
			final int referencesSize = byReferencePhrase.getByReferences().size();

			for (int i = 0; i < referencesSize && i < paramsSize; i++) {
				final ByReference byReference = byReferencePhrase.getByReferences().get(i);
				final CobolAddressGroup addressGroup = params.getReferenceParams().get(i);

				final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
						.getDataDescriptionEntry(byReference.getReferenceCall());
				storageService.putAddressGroup(dataDescriptionEntry, addressGroup, params.getState().getStorage());
			}
		}
	}

	private void initValueParameters(final ByValuePhrase byValuePhrase, final CobolInterpreterParams params) {
		if (byValuePhrase.getByValues() != null && params.getValueParams() != null) {
			final int paramsSize = params.getValueParams().size();
			final int valuesSize = byValuePhrase.getByValues().size();

			for (int i = 0; i < valuesSize && i < paramsSize; i++) {
				final ByValue byValue = byValuePhrase.getByValues().get(i);
				final CobolAddressGroup addressGroup = params.getValueParams().get(i);

				final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
						.getDataDescriptionEntry(byValue.getValueValueStmt());
				storageService.putAddressGroup(dataDescriptionEntry, addressGroup, params.getState().getStorage());
			}
		}
	}

	@Override
	public void run(final ProcedureDivision procedureDivision, final CobolInterpreterParams params) {
		final UsingClause usingClause = procedureDivision.getUsingClause();

		if (usingClause != null) {
			for (final UsingParameter usingParameter : usingClause.getUsingParameters()) {
				final UsingParameterType usingParameterType = usingParameter.getUsingParameterType();

				switch (usingParameterType) {
				case VALUE:
					initValueParameters(usingParameter.getByValuePhrase(), params);
					break;
				case REFERENCE:
				default:
					initReferenceParameters(usingParameter.getByReferencePhrase(), params);
					break;
				}
			}
		}

		final GivingClause givingClause = procedureDivision.getGivingClause();

		if (givingClause != null) {
			initGivingParameter(givingClause, params);
		}

		final List<Statement> statements = procedureDivision.getStatements();
		statementsHandler.run(statements, params);

		final List<Paragraph> paragraphs = procedureDivision.getRootParagraphs();
		runParagraphs(paragraphs, params);

		final List<Section> sections = procedureDivision.getSections();
		runSections(sections, params);
	}

	protected void runParagraphs(final List<Paragraph> paragraphs, final CobolInterpreterParams params) {
		final Iterator<Paragraph> paragraphsIterator = paragraphs.iterator();
		final CobolInterpreterScope scope = params.getState().getStatementsHandlerScopes().push();

		while (!scope.isHalted() && !params.getState().isHalted() && paragraphsIterator.hasNext()) {
			final Paragraph paragraph = paragraphsIterator.next();
			paragraphHandler.run(paragraph, params);
		}

		params.getState().getStatementsHandlerScopes().pop();
	}

	protected void runSections(final List<Section> sections, final CobolInterpreterParams params) {
		final Iterator<Section> sectionsIterator = sections.iterator();
		final CobolInterpreterScope scope = params.getState().getStatementsHandlerScopes().push();

		while (!scope.isHalted() && !params.getState().isHalted() && sectionsIterator.hasNext()) {
			final Section section = sectionsIterator.next();
			sectionHandler.run(section, params);
		}

		params.getState().getStatementsHandlerScopes().pop();
	}
}
