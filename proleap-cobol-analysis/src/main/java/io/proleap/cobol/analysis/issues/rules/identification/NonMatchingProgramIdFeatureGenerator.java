package io.proleap.cobol.analysis.issues.rules.identification;

import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.identification.ProgramIdParagraph;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class NonMatchingProgramIdFeatureGenerator extends FeatureGenerator<ProgramIdParagraph> {

	@Override
	public Stream<ProgramIdParagraph> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.programIdParagraph(compilationUnit).filter(programIdParagraph -> {
			return notSameName(programIdParagraph, compilationUnit);
		});
	}

	protected boolean notSameName(final ProgramIdParagraph programIdParagraph, final CompilationUnit compilationUnit) {
		final String programIdParagraphName = programIdParagraph.getName();
		final String compilationUnitName = compilationUnit.getName();
		return !programIdParagraphName.equalsIgnoreCase(compilationUnitName);
	}
}
