package io.proleap.cobol.analysis.issues.rules.procedure.section;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class FileShouldNotHaveTooManySectionsFeatureGenerator extends FeatureGenerator<ProgramUnit> {

	final static int MAX_SECTIONS = 20;

	@Override
	public Stream<ProgramUnit> getAll(final CompilationUnit compilationUnit) {
		final int size = CobolStreamUtils.sections(compilationUnit).collect(Collectors.toList()).size();

		if (size > MAX_SECTIONS) {
			return Stream.of(compilationUnit.getProgramUnit());
		} else {
			return Stream.empty();
		}
	}
}
