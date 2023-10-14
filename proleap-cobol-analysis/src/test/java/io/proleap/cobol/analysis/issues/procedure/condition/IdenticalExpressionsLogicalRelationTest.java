package io.proleap.cobol.analysis.issues.procedure.condition;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import jakarta.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.procedure.condition.IdenticalExpressionsLogicalRelationFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class IdenticalExpressionsLogicalRelationTest extends TestBase {

	@Inject
	private IdenticalExpressionsLogicalRelationFeatureGenerator feature;

	@Test
	public void testIdentifier() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/condition/IdenticalExpressionsLogicalRelationIdentifier.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program
				.getCompilationUnit("IdenticalExpressionsLogicalRelationIdentifier");
		final List<RelationConditionValueStmt> statements = feature.getAll(compilationUnit)
				.collect(Collectors.toList());

		assertEquals(4, statements.size());
	}

	@Test
	public void testLiteral() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/condition/IdenticalExpressionsLogicalRelationLiteral.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program
				.getCompilationUnit("IdenticalExpressionsLogicalRelationLiteral");
		final List<RelationConditionValueStmt> statements = feature.getAll(compilationUnit)
				.collect(Collectors.toList());

		assertEquals(2, statements.size());
	}

	@Test
	public void testMixed() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/condition/IdenticalExpressionsLogicalRelationMixed.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("IdenticalExpressionsLogicalRelationMixed");
		final List<RelationConditionValueStmt> statements = feature.getAll(compilationUnit)
				.collect(Collectors.toList());

		assertEquals(3, statements.size());
	}
}
