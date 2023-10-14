package io.proleap.cobol.analysis.issues.data;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import jakarta.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.data.DataItemsShouldBeInitWithCorrectTypeFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class DataItemsShouldBeInitWithCorrectTypeTest extends TestBase {

	@Inject
	private DataItemsShouldBeInitWithCorrectTypeFeatureGenerator feature;

	@Test
	public void testCorrect() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/data/InitWithCorrectType.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("InitWithCorrectType");
		final List<DataDescriptionEntry> statements = feature.getAll(compilationUnit).collect(Collectors.toList());
		assertEquals(0, statements.size());
	}

	@Test
	public void testNotCorrect() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/data/InitWithNotCorrectType.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("InitWithNotCorrectType");
		final List<DataDescriptionEntry> statements = feature.getAll(compilationUnit).collect(Collectors.toList());
		assertEquals(6, statements.size());
	}
}
