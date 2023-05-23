package io.proleap.cobol.interpreter.procedure.inspect;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.math.BigDecimal;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.interpreter.TestBase;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class InspectTallyingStatementTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/procedure/inspect/InspectTallyingStatement.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();
		final CobolState state = cobolInterpreterRunner.run(programUnit, createInterpreterParams(parserParams));

		assertEquals(BigDecimal.valueOf(15), valueService.getDecimal(getValue("WS-CNT1", programUnit, state)));
		assertEquals(BigDecimal.valueOf(4), valueService.getDecimal(getValue("WS-CNT2", programUnit, state)));
		assertEquals(BigDecimal.valueOf(10), valueService.getDecimal(getValue("WS-CNT3", programUnit, state)));
		assertEquals(BigDecimal.valueOf(6), valueService.getDecimal(getValue("WS-CNT4", programUnit, state)));
		assertEquals(BigDecimal.valueOf(4), valueService.getDecimal(getValue("WS-CNT5", programUnit, state)));
		assertEquals(BigDecimal.valueOf(0), valueService.getDecimal(getValue("WS-CNT6", programUnit, state)));
		assertEquals(BigDecimal.valueOf(4), valueService.getDecimal(getValue("WS-CNT7", programUnit, state)));
		assertEquals(BigDecimal.valueOf(3), valueService.getDecimal(getValue("WS-CNT8", programUnit, state)));
	}
}
