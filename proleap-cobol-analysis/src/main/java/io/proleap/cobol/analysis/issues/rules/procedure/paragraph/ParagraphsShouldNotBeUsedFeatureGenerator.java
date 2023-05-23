package io.proleap.cobol.analysis.issues.rules.procedure.paragraph;

import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class ParagraphsShouldNotBeUsedFeatureGenerator extends FeatureGenerator<Paragraph> {

	@Override
	public Stream<Paragraph> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.paragraphs(compilationUnit);
	}
}
