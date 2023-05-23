package io.proleap.cobol.interpreter.handlers.data.impl;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.DataDivision;
import io.proleap.cobol.asg.metamodel.data.communication.CommunicationSection;
import io.proleap.cobol.asg.metamodel.data.file.FileSection;
import io.proleap.cobol.asg.metamodel.data.localstorage.LocalStorageSection;
import io.proleap.cobol.asg.metamodel.data.workingstorage.WorkingStorageSection;
import io.proleap.cobol.interpreter.handlers.data.DataDivisionHandler;
import io.proleap.cobol.interpreter.handlers.data.communication.CommunicationSectionHandler;
import io.proleap.cobol.interpreter.handlers.data.file.FileSectionHandler;
import io.proleap.cobol.interpreter.handlers.data.localstorage.LocalStorageSectionHandler;
import io.proleap.cobol.interpreter.handlers.data.workingstorage.WorkingStorageSectionHandler;
import io.proleap.cobol.interpreter.handlers.impl.AbstractCobolInterpreterHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class DataDivisionHandlerImpl extends AbstractCobolInterpreterHandler implements DataDivisionHandler {

	@Inject
	private CommunicationSectionHandler communicationSectionHandler;

	@Inject
	private FileSectionHandler fileSectionHandler;

	@Inject
	private LocalStorageSectionHandler localStorageSectionHandler;

	@Inject
	private WorkingStorageSectionHandler workingStorageSectionHandler;

	@Override
	public void run(final DataDivision dataDivision, final CobolInterpreterParams params) {
		final CommunicationSection communicationSection = dataDivision.getCommunicationSection();
		final LocalStorageSection localStorageSection = dataDivision.getLocalStorageSection();
		final FileSection fileSection = dataDivision.getFileSection();
		final WorkingStorageSection workingStorageSection = dataDivision.getWorkingStorageSection();

		if (communicationSection != null) {
			communicationSectionHandler.run(communicationSection, params);
		}

		if (localStorageSection != null) {
			localStorageSectionHandler.run(localStorageSection, params);
		}

		if (fileSection != null) {
			fileSectionHandler.run(fileSection, params);
		}

		if (workingStorageSection != null) {
			workingStorageSectionHandler.run(workingStorageSection, params);
		}
	}
}
