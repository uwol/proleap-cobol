package io.proleap.cobol.analysis.issues.procedure.perform;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.procedure.perform.NumberOfPerformStatementsShouldBeLimitedFeatureGenerator;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class NumberOfPerformStatementsShouldBeLimitedTest extends TestBase {

	@Inject
	private NumberOfPerformStatementsShouldBeLimitedFeatureGenerator feature;

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/perform/NumberOfPerformStatementsShouldBeLimited.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("NumberOfPerformStatementsShouldBeLimited");
		final List<ASGElement> elements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(2, elements.size());

		assertEquals("NumberOfPerformStatementsShouldBeLimited",
				elements.get(0).getProgram().getCompilationUnits().get(0).getName());
	}
}
