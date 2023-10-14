package io.proleap.cobol.analysis.issues.rules.procedure.paragraph;

import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class ParagraphNamingConventionFeatureGenerator extends FeatureGenerator<Paragraph> {

	private static final String NAMING_CONVENTION = "[A-Z-]+";

	@Override
	public Stream<Paragraph> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.paragraphs(compilationUnit).filter(paragraph -> {
			return !isFollowingANamingConvention(paragraph);
		});
	}

	protected boolean isFollowingANamingConvention(final Paragraph paragraph) {
		if (paragraph == null || !paragraph.getName().matches(NAMING_CONVENTION)) {
			return false;
		}

		return true;
	}
}
