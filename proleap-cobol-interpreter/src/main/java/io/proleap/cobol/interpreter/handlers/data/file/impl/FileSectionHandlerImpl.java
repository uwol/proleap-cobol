package io.proleap.cobol.interpreter.handlers.data.file.impl;

import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.file.FileSection;
import io.proleap.cobol.interpreter.handlers.data.file.FileSectionHandler;
import io.proleap.cobol.interpreter.handlers.data.impl.AbstractDataDescriptionEntryContainerInitializer;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class FileSectionHandlerImpl extends AbstractDataDescriptionEntryContainerInitializer
		implements FileSectionHandler {

	@Override
	public void run(final FileSection fileSection, final CobolInterpreterParams params) {
		for (final FileDescriptionEntry fileDescriptionEntry : fileSection.getFileDescriptionEntries()) {
			initDataDescriptionEntryContainer(fileDescriptionEntry, params.getState().getStorage());
		}
	}
}
