package io.proleap.cobol.interpreter.valuestmt.relation;

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
public class ConditionNotEqualTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/valuestmt/relation/ConditionNotEqual.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();
		final CobolState state = cobolInterpreterRunner.run(programUnit, createInterpreterParams(parserParams));

		assertEquals(10, state.getConsole().getLines().size());

		assertEquals("different string", state.getConsole().getLines().get(0));
		assertEquals("different string", state.getConsole().getLines().get(1));

		assertEquals("different decimal", state.getConsole().getLines().get(2));
		assertEquals("different decimal", state.getConsole().getLines().get(3));

		assertEquals("different boolean", state.getConsole().getLines().get(4));
		assertEquals("different boolean", state.getConsole().getLines().get(5));

		assertEquals("high-value low-value", state.getConsole().getLines().get(6));
		assertEquals("high-value low-value", state.getConsole().getLines().get(7));

		assertEquals("low-value high-value", state.getConsole().getLines().get(8));
		assertEquals("low-value high-value", state.getConsole().getLines().get(9));
	}
}
