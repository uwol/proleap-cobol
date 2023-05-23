package io.proleap.cobol.analysis.issues.procedure.condition;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.procedure.condition.RelatedWhenIfStatementsSameConditionsFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class RelatedWhenIfStatementsSameConditionsTest extends TestBase {

	@Inject
	private RelatedWhenIfStatementsSameConditionsFeatureGenerator feature;

	@Test
	public void testEvaluateWhen() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/condition/RelatedWhenStatementSameConditions.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("RelatedWhenStatementSameConditions");
		final List<Statement> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(1, statements.size());
	}

	// TODO: Test for If-Else Clause needs to be implemented
	@Test
	public void testUsageClause() throws Exception {
		return;
	}
}
