package io.proleap.cobol.analysis.issues.rules.procedure.paragraph;

import java.util.List;
import java.util.stream.Stream;
import java.util.stream.Stream.Builder;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class ParagraphsShouldNotHaveTooManyStatementsFeatureGenerator extends FeatureGenerator<Paragraph> {

	final static int MAX_STATEMENTS = 10;

	@Override
	public Stream<Paragraph> getAll(final CompilationUnit compilationUnit) {
		final Stream<Paragraph> paragraphs = CobolStreamUtils.paragraphs(compilationUnit);

		return relevantParagraphs(paragraphs);
	}

	protected Stream<Paragraph> relevantParagraphs(final Stream<Paragraph> paragraphs) {
		final Builder<Paragraph> streamBuilder = Stream.builder();

		paragraphs.forEach(paragraph -> {
			final List<Statement> statements = paragraph.getStatements();

			if (statements != null) {
				if (statements.size() > MAX_STATEMENTS) {
					streamBuilder.add(paragraph);
				}
			}
		});

		return streamBuilder.build();
	}

}
