package io.proleap.cobol.analysis.issues.rules.identification;

import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.identification.ProgramIdParagraph;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class ProgramNamingConventionFeatureGenerator extends FeatureGenerator<ProgramIdParagraph> {

	private static final String NAMING_CONVENTION = "^([A-Z0-9_]*|[a-z0-9_]*)$";

	@Override
	public Stream<ProgramIdParagraph> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.programIdParagraph(compilationUnit).filter(programIdParagraph -> {
			return !isRelevantParagraph(programIdParagraph);
		});
	}

	protected boolean isMatchingNamingConvention(final String name) {
		if (name != null && name.matches(NAMING_CONVENTION)) {
			return true;
		}

		return false;
	}

	protected boolean isRelevantParagraph(final ProgramIdParagraph programIdParagraph) {
		return isMatchingNamingConvention(programIdParagraph.getName());
	}
}
