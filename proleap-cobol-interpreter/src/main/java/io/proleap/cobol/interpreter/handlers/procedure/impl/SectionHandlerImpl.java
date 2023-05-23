package io.proleap.cobol.interpreter.handlers.procedure.impl;

import java.util.Iterator;
import java.util.List;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.interpreter.handlers.impl.AbstractCobolInterpreterHandler;
import io.proleap.cobol.interpreter.handlers.procedure.ParagraphHandler;
import io.proleap.cobol.interpreter.handlers.procedure.SectionHandler;
import io.proleap.cobol.interpreter.handlers.procedure.StatementsHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class SectionHandlerImpl extends AbstractCobolInterpreterHandler implements SectionHandler {

	@Inject
	private ParagraphHandler paragraphHandler;

	@Inject
	private StatementsHandler statementsHandler;

	@Override
	public void run(final Section section, final CobolInterpreterParams params) {
		final List<Statement> statements = section.getStatements();
		statementsHandler.run(statements, params);

		final List<Paragraph> paragraphs = section.getParagraphs();
		final Iterator<Paragraph> iterator = paragraphs.iterator();

		while (!params.getState().isHalted() && iterator.hasNext()) {
			final Paragraph paragraph = iterator.next();
			paragraphHandler.run(paragraph, params);
		}
	}
}
