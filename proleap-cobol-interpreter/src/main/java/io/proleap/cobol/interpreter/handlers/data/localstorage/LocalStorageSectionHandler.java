package io.proleap.cobol.interpreter.handlers.data.localstorage;

import io.proleap.cobol.asg.metamodel.data.localstorage.LocalStorageSection;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public interface LocalStorageSectionHandler {

	void run(LocalStorageSection localStorageSection, CobolInterpreterParams params);
}
