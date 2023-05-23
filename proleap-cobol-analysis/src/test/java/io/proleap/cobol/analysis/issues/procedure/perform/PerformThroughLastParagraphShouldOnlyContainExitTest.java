package io.proleap.cobol.analysis.issues.procedure.perform;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.procedure.perform.PerformThroughLastParagraphShouldOnlyContainExitFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class PerformThroughLastParagraphShouldOnlyContainExitTest extends TestBase {

	@Inject
	private PerformThroughLastParagraphShouldOnlyContainExitFeatureGenerator feature;

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/perform/PerformThroughLastParagraphShouldOnlyContainExit.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program
				.getCompilationUnit("PerformThroughLastParagraphShouldOnlyContainExit");
		final List<Statement> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(2, statements.size());
	}
}
