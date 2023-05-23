package io.proleap.cobol.analysis.issues.data;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.data.BinaryVariablesShouldDeclareTableSizesFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class BinaryVariablesShouldDeclareTableSizesTest extends TestBase {

	@Inject
	private BinaryVariablesShouldDeclareTableSizesFeatureGenerator feature;

	@Test
	public void testBinary() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/data/BinaryVariablesDeclareTableSizes.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("BinaryVariablesDeclareTableSizes");
		final List<DataDescriptionEntry> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(0, statements.size());
	}

	@Test
	public void testNonBinary() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/data/NotBinaryVariablesDeclareTableSizes.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("NotBinaryVariablesDeclareTableSizes");
		final List<DataDescriptionEntry> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(3, statements.size());
	}
}
