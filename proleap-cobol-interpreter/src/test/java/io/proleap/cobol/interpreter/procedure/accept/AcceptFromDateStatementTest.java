package io.proleap.cobol.interpreter.procedure.accept;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;

import org.junit.jupiter.api.Test;

import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.interpreter.TestBase;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class AcceptFromDateStatementTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/procedure/accept/AcceptFromDateStatement.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();
		final CobolState state = cobolInterpreterRunner.run(programUnit, createInterpreterParams(parserParams));

		assertEquals(13, state.getConsole().getLines().size());
		assertEquals(6, state.getConsole().getLines().get(0).trim().length());
		assertEquals(8, state.getConsole().getLines().get(1).trim().length());
		assertEquals(5, state.getConsole().getLines().get(2).trim().length());
		assertEquals(7, state.getConsole().getLines().get(3).trim().length());
		assertEquals(0, state.getConsole().getLines().get(4).trim().length());
		assertEquals(8, state.getConsole().getLines().get(5).trim().length());
		assertEquals(0, state.getConsole().getLines().get(6).trim().length());
		assertEquals(0, state.getConsole().getLines().get(7).trim().length());
		assertEquals(0, state.getConsole().getLines().get(8).trim().length());
		assertEquals(0, state.getConsole().getLines().get(9).trim().length());
		assertEquals(4, state.getConsole().getLines().get(10).trim().length());
		assertEquals(8, state.getConsole().getLines().get(11).trim().length());
		assertEquals(7, state.getConsole().getLines().get(12).trim().length());
	}
}
