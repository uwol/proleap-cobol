package io.proleap.cobol.interpreter.handlers.data.localstorage.impl;

import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.localstorage.LocalStorageSection;
import io.proleap.cobol.interpreter.handlers.data.impl.AbstractDataDescriptionEntryContainerInitializer;
import io.proleap.cobol.interpreter.handlers.data.localstorage.LocalStorageSectionHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class LocalStorageSectionHandlerImpl extends AbstractDataDescriptionEntryContainerInitializer
		implements LocalStorageSectionHandler {

	@Override
	public void run(final LocalStorageSection localStorageSection, final CobolInterpreterParams params) {
		initDataDescriptionEntryContainer(localStorageSection, params.getState().getStorage());
	}
}
