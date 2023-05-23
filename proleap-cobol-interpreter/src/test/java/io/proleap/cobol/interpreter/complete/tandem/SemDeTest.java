package io.proleap.cobol.interpreter.complete.tandem;

import java.io.File;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.interpreter.TestBase;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class SemDeTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File("src/test/resources/io/proleap/cobol/interpreter/complete/tandem/SemDe.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();
		cobolInterpreterRunner.run(programUnit, createInterpreterParams(parserParams));
	}
}
