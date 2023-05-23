package io.proleap.cobol.interpreter.handlers.procedure;

import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public interface ParagraphHandler {

	void run(Paragraph paragraph, CobolInterpreterParams params);
}
