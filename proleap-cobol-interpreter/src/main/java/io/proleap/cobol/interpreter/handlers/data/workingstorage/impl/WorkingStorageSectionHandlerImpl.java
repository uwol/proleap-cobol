package io.proleap.cobol.interpreter.handlers.data.workingstorage.impl;

import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.workingstorage.WorkingStorageSection;
import io.proleap.cobol.interpreter.handlers.data.impl.AbstractDataDescriptionEntryContainerInitializer;
import io.proleap.cobol.interpreter.handlers.data.workingstorage.WorkingStorageSectionHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class WorkingStorageSectionHandlerImpl extends AbstractDataDescriptionEntryContainerInitializer
		implements WorkingStorageSectionHandler {

	@Override
	public void run(final WorkingStorageSection workingStorageSection, final CobolInterpreterParams params) {
		initDataDescriptionEntryContainer(workingStorageSection, params.getState().getStorage());
	}
}
