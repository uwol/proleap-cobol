package io.proleap.cobol.interpreter.procedure.inspect;

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
public class InspectReplacingStatementTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/procedure/inspect/InspectReplacingStatement.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();
		final CobolState state = cobolInterpreterRunner.run(programUnit, createInterpreterParams(parserParams));

		assertEquals("EEEEEEEEEE", valueService.getString(getValue("WS-STRING1", programUnit, state)));
		assertEquals("AAAABEEEEE", valueService.getString(getValue("WS-STRING2", programUnit, state)));
		assertEquals("AAAABBBEED", valueService.getString(getValue("WS-STRING3", programUnit, state)));
		assertEquals("AAAABBBECD", valueService.getString(getValue("WS-STRING4", programUnit, state)));
		assertEquals("EEEEBBBCCD", valueService.getString(getValue("WS-STRING5", programUnit, state)));
	}
}
