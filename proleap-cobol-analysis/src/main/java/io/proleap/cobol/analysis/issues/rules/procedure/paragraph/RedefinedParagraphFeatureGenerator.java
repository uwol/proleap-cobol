package io.proleap.cobol.analysis.issues.rules.procedure.paragraph;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class RedefinedParagraphFeatureGenerator extends FeatureGenerator<Paragraph> {

	protected List<Paragraph> findParagraphsWithoutSection(final ProcedureDivision procedureDivision) {
		final List<Paragraph> result = new ArrayList<Paragraph>();
		final List<Paragraph> paragraphs = procedureDivision.getParagraphs();

		for (final Paragraph paragraph : paragraphs) {
			if (paragraph.getSection() == null) {
				result.add(paragraph);
			}
		}

		return result;
	}

	protected List<Paragraph> findRedefinedParagraphs(final List<Paragraph> paragraphs) {
		final List<Paragraph> result = new ArrayList<Paragraph>();
		final Set<String> names = new HashSet<String>();

		for (final Paragraph paragraph : paragraphs) {
			final String name = paragraph.getName();

			if (names.contains(name)) {
				result.add(paragraph);
			} else {
				names.add(name);
			}
		}

		return result;
	}

	protected List<Paragraph> findRedefinedParagraphs(final ProcedureDivision procedureDivision) {
		final List<Paragraph> redefinedParagraphs = new ArrayList<Paragraph>();
		final List<Section> sections = procedureDivision.getSections();

		for (final Section section : sections) {
			final List<Paragraph> paragraphs = section.getParagraphs();
			redefinedParagraphs.addAll(findRedefinedParagraphs(paragraphs));
		}

		final List<Paragraph> paragraphsWithoutSection = findParagraphsWithoutSection(procedureDivision);
		redefinedParagraphs.addAll(findRedefinedParagraphs(paragraphsWithoutSection));

		return redefinedParagraphs;
	}

	@Override
	public Stream<Paragraph> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.procedureDivisions(compilationUnit).flatMap(procedureDivision -> {
			return findRedefinedParagraphs(procedureDivision).stream();
		});
	}
}
