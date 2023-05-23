package io.proleap.cobol.analysis.issues.procedure.condition;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.procedure.condition.IdenticalExpressionsAndOrConditionFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class IdenticalExpressionsAndOrConditionTest extends TestBase {

	@Inject
	private IdenticalExpressionsAndOrConditionFeatureGenerator feature;

	@Test
	public void testIdentifier() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/condition/IdenticalExpressionsAndOrConditionIdentifier.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program
				.getCompilationUnit("IdenticalExpressionsAndOrConditionIdentifier");
		final List<Statement> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(2, statements.size());
	}

	@Test
	public void testLiterals() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/condition/IdenticalExpressionsAndOrConditionLiterals.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program
				.getCompilationUnit("IdenticalExpressionsAndOrConditionLiterals");
		final List<Statement> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(2, statements.size());
	}

	@Test
	public void testMixed() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/condition/IdenticalExpressionsAndOrConditionMixed.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("IdenticalExpressionsAndOrConditionMixed");
		final List<Statement> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(1, statements.size());
	}
}
