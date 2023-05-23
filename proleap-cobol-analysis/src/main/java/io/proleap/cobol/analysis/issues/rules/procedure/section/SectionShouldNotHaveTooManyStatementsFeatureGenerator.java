package io.proleap.cobol.analysis.issues.rules.procedure.section;

import java.util.List;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class SectionShouldNotHaveTooManyStatementsFeatureGenerator extends FeatureGenerator<Section> {

	final static int MAX_STATEMENTS = 50;

	@Override
	public Stream<Section> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.sections(compilationUnit).filter(section -> isRelevantSection(section));
	}

	protected boolean isRelevantSection(final Section section) {
		final boolean result;
		final List<Paragraph> paragraphs = section.getParagraphs();

		final List<Statement> sectionStatements = section.getStatements();
		int statementSum = sectionStatements.size();

		if (!paragraphs.isEmpty()) {
			for (final Paragraph paragraph : paragraphs) {
				final List<Statement> paragraphStatements = paragraph.getStatements();

				if (!paragraphStatements.isEmpty()) {
					statementSum = statementSum + paragraphStatements.size();
				}
			}
		}

		if (statementSum <= MAX_STATEMENTS) {
			result = false;
		} else {
			result = true;
		}

		return result;
	}
}
