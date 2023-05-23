package io.proleap.cobol.analysis.issues.procedure.condition;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.procedure.condition.VariablesOfDifferentNumericFormatFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class VariablesOfDifferentNumericFormatTest extends TestBase {

	@Inject
	private VariablesOfDifferentNumericFormatFeatureGenerator feature;

	@Test
	public void testDecimal() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/condition/VariablesOfDifferentNumericFormatDecimal.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("VariablesOfDifferentNumericFormatDecimal");
		final List<RelationConditionValueStmt> statements = feature.getAll(compilationUnit)
				.collect(Collectors.toList());

		assertEquals(2, statements.size());
	}

	@Test
	public void testUsageClause() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/condition/VariablesOfDifferentNumericFormatUsageClause.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program
				.getCompilationUnit("VariablesOfDifferentNumericFormatUsageClause");
		final List<RelationConditionValueStmt> statements = feature.getAll(compilationUnit)
				.collect(Collectors.toList());

		assertEquals(2, statements.size());
	}
}
