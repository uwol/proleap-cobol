package io.proleap.cobol.interpreter.procedure.accept;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.interpreter.TestBase;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class AcceptNoFromStatementTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/procedure/accept/AcceptNoFromStatement.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();

		final CobolInterpreterParams interpreterParams = createInterpreterParams(parserParams);
		interpreterParams.setAcceptParams(Arrays.asList("test"));

		final CobolState state = cobolInterpreterRunner.run(programUnit, interpreterParams);
		assertEquals(1, state.getConsole().getLines().size());
		assertEquals("test      ", state.getConsole().getLines().get(0));
	}
}
