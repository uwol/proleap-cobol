package io.proleap.cobol.analysis.issues.rules.procedure.paragraph;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class UncalledParagraphFeatureGenerator extends FeatureGenerator<Paragraph> {

	protected List<Paragraph> findParagraphsThatShouldBeCalled(final ProcedureDivision procedureDivision) {
		final List<Paragraph> paragraphs = procedureDivision.getParagraphs();
		final List<Paragraph> result;

		if (!procedureDivision.getStatements().isEmpty()) {
			result = paragraphs;
		} else if (!paragraphs.isEmpty()) {
			result = paragraphs.subList(1, paragraphs.size());
		} else {
			result = new ArrayList<Paragraph>();
		}

		return result;
	}

	@Override
	public Stream<Paragraph> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.procedureDivisions(compilationUnit).flatMap(procedureDivision -> {
			return findParagraphsThatShouldBeCalled(procedureDivision).stream();
		}).filter(paragraph -> {
			return paragraph.getCalls().isEmpty();
		});
	}
}
