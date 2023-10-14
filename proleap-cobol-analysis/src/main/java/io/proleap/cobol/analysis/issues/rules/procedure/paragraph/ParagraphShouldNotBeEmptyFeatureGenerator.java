package io.proleap.cobol.analysis.issues.rules.procedure.paragraph;

import java.util.List;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class ParagraphShouldNotBeEmptyFeatureGenerator extends FeatureGenerator<Paragraph> {

	@Override
	public Stream<Paragraph> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.paragraphs(compilationUnit).filter(paragraph -> isEmptyParagraph(paragraph));
	}

	protected boolean isEmptyParagraph(final Paragraph paragraph) {
		final List<Statement> statements = paragraph.getStatements();

		return statements.isEmpty();
	}
}
