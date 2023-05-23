package io.proleap.cobol.analysis.issues.data;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.data.BinaryVariablesShouldBeUsedForTableSubscriptAccessFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.call.TableCall;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class BinaryVariablesShouldBeUsedForTableSubscriptAccessTest extends TestBase {

	@Inject
	private BinaryVariablesShouldBeUsedForTableSubscriptAccessFeatureGenerator feature;

	@Test
	public void testBinary() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/data/BinaryTableSubscriptAccess.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("BinaryTableSubscriptAccess");
		final List<TableCall> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(0, statements.size());
	}

	@Test
	public void testNonBinary() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/data/NonBinaryTableSubscriptAccess.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("NonBinaryTableSubscriptAccess");
		final List<TableCall> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(5, statements.size());
	}
}
