package io.proleap.cobol.interpreter.handlers.data.communication.impl;

import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.communication.CommunicationSection;
import io.proleap.cobol.interpreter.handlers.data.communication.CommunicationSectionHandler;
import io.proleap.cobol.interpreter.handlers.data.impl.AbstractDataDescriptionEntryContainerInitializer;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class CommunicationSectionHandlerImpl extends AbstractDataDescriptionEntryContainerInitializer
		implements CommunicationSectionHandler {

	@Override
	public void run(final CommunicationSection communicationSection, final CobolInterpreterParams params) {
		initDataDescriptionEntryContainer(communicationSection, params.getState().getStorage());
	}
}
