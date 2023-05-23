package io.proleap.cobol.analysis.issues.procedure.paragraph;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.procedure.paragraph.ParagraphsShouldNotHaveTooManyStatementsFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class ParagraphsDoesNotHaveTooManyStatementsTest extends TestBase {

	@Inject
	private ParagraphsShouldNotHaveTooManyStatementsFeatureGenerator feature;

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/paragraph/ParagraphHasNotTooManyStatements.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("ParagraphHasNotTooManyStatements");
		final List<Paragraph> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(0, statements.size());
	}
}
