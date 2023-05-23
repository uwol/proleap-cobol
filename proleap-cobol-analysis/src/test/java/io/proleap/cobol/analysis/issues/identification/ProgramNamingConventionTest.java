package io.proleap.cobol.analysis.issues.identification;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.identification.ProgramNamingConventionFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.identification.ProgramIdParagraph;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class ProgramNamingConventionTest extends TestBase {

	@Inject
	private ProgramNamingConventionFeatureGenerator feature;

	@Test
	public void test() throws Exception {
		final File inputFile1 = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/identification/ProgramNotFollowingNamingConvention.cbl");
		final Program program1 = new CobolParserRunnerImpl().analyzeFile(inputFile1, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit1 = program1.getCompilationUnit("ProgramNotFollowingNamingConvention");
		final List<ProgramIdParagraph> programIds1 = feature.getAll(compilationUnit1).collect(Collectors.toList());

		assertEquals(1, programIds1.size());

		final File inputFile2 = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/identification/ProgramFollowingNamingConvention.cbl");
		final Program program2 = new CobolParserRunnerImpl().analyzeFile(inputFile2, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit2 = program2.getCompilationUnit("ProgramFollowingNamingConvention");
		final List<ProgramIdParagraph> programIds2 = feature.getAll(compilationUnit2).collect(Collectors.toList());

		assertEquals(0, programIds2.size());
	}
}
