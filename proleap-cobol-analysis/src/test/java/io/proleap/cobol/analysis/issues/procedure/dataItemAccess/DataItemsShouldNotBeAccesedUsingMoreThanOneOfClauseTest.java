package io.proleap.cobol.analysis.issues.procedure.dataItemAccess;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.procedure.dataItemAccess.DataItemsShouldNotBeAccesedUsingMoreThanOneOfClauseFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class DataItemsShouldNotBeAccesedUsingMoreThanOneOfClauseTest extends TestBase {

	@Inject
	private DataItemsShouldNotBeAccesedUsingMoreThanOneOfClauseFeatureGenerator displayFeature;

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/dataItemAccess/AccesedUsingMoreThanOneOfClause.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("AccesedUsingMoreThanOneOfClause");
		final List<Call> statements = displayFeature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(2, statements.size());
	}
}
