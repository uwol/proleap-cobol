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
public class SectionEmptyFeatureGenerator extends FeatureGenerator<Section> {

	@Override
	public Stream<Section> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.sections(compilationUnit).filter(section -> isRelevantSection(section));
	}

	protected Boolean isRelevantSection(final Section section) {
		Boolean result = true;
		if (section == null) {
			result = false;
		} else {
			final List<Paragraph> paragraphs = section.getParagraphs();

			if (paragraphs != null && !paragraphs.isEmpty()) {
				paragraphs.removeIf(
						paragraph -> paragraph.getStatements() == null || paragraph.getStatements().isEmpty());
				result = paragraphs.isEmpty();
			}
			final List<Statement> statements = section.getStatements();

			if (statements != null && !statements.isEmpty()) {
				result = false;
			}
		}
		return result;
	}
}
