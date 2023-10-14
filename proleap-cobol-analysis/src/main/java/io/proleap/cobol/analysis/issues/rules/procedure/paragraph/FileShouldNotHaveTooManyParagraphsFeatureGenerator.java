package io.proleap.cobol.analysis.issues.rules.procedure.paragraph;

import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class FileShouldNotHaveTooManyParagraphsFeatureGenerator extends FeatureGenerator<ProgramUnit> {

	final static int MAX_PARAGRAPHS = 50;

	@Override
	public Stream<ProgramUnit> getAll(final CompilationUnit compilationUnit) {
		final long size = CobolStreamUtils.paragraphs(compilationUnit).count();

		if (size > MAX_PARAGRAPHS) {
			return Stream.of(compilationUnit.getProgramUnit());
		} else {
			return Stream.empty();
		}
	}
}
