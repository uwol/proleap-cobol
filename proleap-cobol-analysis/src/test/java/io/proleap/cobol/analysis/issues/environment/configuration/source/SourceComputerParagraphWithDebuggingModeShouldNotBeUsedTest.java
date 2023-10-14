package io.proleap.cobol.analysis.issues.environment.configuration.source;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import jakarta.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.environment.configuration.source.SourceComputerParagraphWithDebuggingModeShouldNotBeUsedFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.environment.configuration.source.SourceComputerParagraph;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class SourceComputerParagraphWithDebuggingModeShouldNotBeUsedTest extends TestBase {

	@Inject
	private SourceComputerParagraphWithDebuggingModeShouldNotBeUsedFeatureGenerator paragraph;

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/environment/configuration/source/SourceComputerParagraphWithDebuggingMode.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("SourceComputerParagraphWithDebuggingMode");
		final List<SourceComputerParagraph> statements = paragraph.getAll(compilationUnit).collect(Collectors.toList());
		assertEquals(1, statements.size());
	}
}
