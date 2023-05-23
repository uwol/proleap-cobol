package io.proleap.cobol.analysis.issues.procedure.call;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.analysis.TestBase;
import io.proleap.cobol.analysis.issues.rules.procedure.call.CallParametersShouldBeInitializedFeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class CallParametersShouldBeInitializedTest extends TestBase {

	@Inject
	private CallParametersShouldBeInitializedFeatureGenerator feature;

	@Test
	public void testLinkage() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/call/CallParameterInitLinkage.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("CallParameterInitLinkage");
		final List<Statement> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(1, statements.size());
	}

	@Test
	public void testMoveInit() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/call/CallParameterInitMoveInit.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("CallParameterInitMoveInit");
		final List<Statement> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(2, statements.size());
	}

	@Test
	public void testParent() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/call/CallParameterInitParent.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("CallParameterInitParent");
		final List<Statement> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(2, statements.size());
	}

	@Test
	public void testValue() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/analysis/issues/procedure/call/CallParameterInitValue.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.TANDEM);
		final CompilationUnit compilationUnit = program.getCompilationUnit("CallParameterInitValue");
		final List<Statement> statements = feature.getAll(compilationUnit).collect(Collectors.toList());

		assertEquals(2, statements.size());
	}
}
