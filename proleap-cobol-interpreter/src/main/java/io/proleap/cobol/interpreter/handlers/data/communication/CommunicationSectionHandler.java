package io.proleap.cobol.interpreter.handlers.data.communication;

import io.proleap.cobol.asg.metamodel.data.communication.CommunicationSection;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public interface CommunicationSectionHandler {

	void run(CommunicationSection communicationSection, CobolInterpreterParams params);
}
