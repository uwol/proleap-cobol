package io.proleap.cobol.analysis.issues.rules.environment.configuration.source;

import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.environment.configuration.source.SourceComputerParagraph;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class SourceComputerParagraphWithDebuggingModeShouldNotBeUsedFeatureGenerator
		extends FeatureGenerator<SourceComputerParagraph> {

	@Override
	public Stream<SourceComputerParagraph> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.sourceComputerParagraphs(compilationUnit)
				.filter(paragraph -> paragraph.isDebuggingMode());
	}
}
