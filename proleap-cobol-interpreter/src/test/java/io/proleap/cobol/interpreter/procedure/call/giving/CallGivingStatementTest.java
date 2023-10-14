package io.proleap.cobol.interpreter.procedure.call.giving;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.math.BigDecimal;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.interpreter.TestBase;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class CallGivingStatementTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/procedure/call/giving/CallGivingStatement.cbl");
		final File calledFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/procedure/call/giving/Util.cbl");

		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();

		final CobolInterpreterParams interpreterParams = createInterpreterParams(parserParams);
		interpreterParams.setCobolFiles(Arrays.asList(calledFile));

		final CobolState state = cobolInterpreterRunner.run(programUnit, interpreterParams);

		assertEquals(4, state.getConsole().getLines().size());
		assertEquals("Student Id : 1000", state.getConsole().getLines().get(0));
		assertEquals("In Called Program", state.getConsole().getLines().get(1));
		assertEquals("Student Id : 1111", state.getConsole().getLines().get(2));
		assertEquals("Student Id : 1111", state.getConsole().getLines().get(3));

		assertEquals(BigDecimal.valueOf(1111), valueService.getDecimal(getValue("WS-STUDENT-ID", programUnit, state)));
	}
}
