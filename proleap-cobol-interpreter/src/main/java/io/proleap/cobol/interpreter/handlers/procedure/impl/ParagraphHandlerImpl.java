package io.proleap.cobol.interpreter.handlers.procedure.impl;

import java.util.List;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.interpreter.handlers.impl.AbstractCobolInterpreterHandler;
import io.proleap.cobol.interpreter.handlers.procedure.ParagraphHandler;
import io.proleap.cobol.interpreter.handlers.procedure.StatementsHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class ParagraphHandlerImpl extends AbstractCobolInterpreterHandler implements ParagraphHandler {

	@Inject
	private StatementsHandler statementsHandler;

	@Override
	public void run(final Paragraph paragraph, final CobolInterpreterParams params) {
		final List<Statement> statements = paragraph.getStatements();
		statementsHandler.run(statements, params);
	}
}
