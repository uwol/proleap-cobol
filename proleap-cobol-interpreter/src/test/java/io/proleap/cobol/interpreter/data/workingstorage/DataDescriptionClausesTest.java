package io.proleap.cobol.interpreter.data.workingstorage;

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
public class DataDescriptionClausesTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/data/workingstorage/DataDescriptionClauses.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();
		final CobolState state = cobolInterpreterRunner.run(programUnit, createInterpreterParams(parserParams));

		assertEquals(" ".repeat(10), valueService.getString(getValue("ITEM", programUnit, state)));
		assertEquals(" ".repeat(5), valueService.getString(getValue("ITEMRED", programUnit, state)));
		assertEquals(BigDecimal.ZERO, valueService.getDecimal(getValue("ITEMINT", programUnit, state)));
		assertEquals(" ".repeat(10), valueService.getString(getValue("ITEMSTR", programUnit, state)));
		assertEquals(" ".repeat(10), valueService.getString(getValue("ITEMEXT", programUnit, state)));
		assertEquals(" ".repeat(10), valueService.getString(getValue("ITEMGLB", programUnit, state)));
		assertEquals(" ".repeat(10), valueService.getString(getValue("ITEMTPD", programUnit, state)));
		assertEquals(" ".repeat(10), valueService.getString(getValue("ITEMTHR", programUnit, state)));
		assertEquals(BigDecimal.ZERO, valueService.getDecimal(getValue("ITEMPIC", programUnit, state)));
		assertEquals(BigDecimal.ZERO, valueService.getDecimal(getValue("ITEMCOM", programUnit, state)));
		assertEquals(BigDecimal.ZERO, valueService.getDecimal(getValue("ITEMOWN", programUnit, state)));
		assertEquals(BigDecimal.ZERO, valueService.getDecimal(getValue("ITEMLOC", programUnit, state)));
	}
}
