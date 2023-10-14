package io.proleap.cobol.analysis.issues.rules.procedure.section;

import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class SectionShouldNotBeUsedFeatureGenerator extends FeatureGenerator<Section> {

	@Override
	public Stream<Section> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.procedureDivisions(compilationUnit).flatMap(p -> p.getSections().stream());
	}
}
