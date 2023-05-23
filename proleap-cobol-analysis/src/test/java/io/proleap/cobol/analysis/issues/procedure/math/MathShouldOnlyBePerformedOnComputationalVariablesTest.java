package io.proleap.cobol.analysis.issues.procedure.math;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.procedure.math.MathShouldOnlyBePerformedOnComputationalVariablesFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class MathShouldOnlyBePerformedOnComputationalVariablesTest extends TestBase {

	@Inject
	private MathShouldOnlyBePerformedOnComputationalVariablesFeatureGenerator feature;

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/math/MathShouldOnlyBePerformedOnComputationalVariables.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program
				.getCompilationUnit("MathShouldOnlyBePerformedOnComputationalVariables");
		final List<ArithmeticValueStmt> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(8, statements.size());
	}
}
