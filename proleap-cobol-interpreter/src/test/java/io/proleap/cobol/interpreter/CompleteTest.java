package io.proleap.cobol.interpreter;

import java.io.File;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class CompleteTest extends TestBase {

	@Disabled
	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"/Users/uwol/ventures/proleap/proleap-cobol-parser/src/test/resources/gov/nist/CM101M.CBL");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.FIXED;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();
		cobolInterpreterRunner.run(programUnit, createInterpreterParams(parserParams));
	}
}
