package io.proleap.cobol.interpreter.procedure.read;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.interpreter.TestBase;
import io.proleap.cobol.interpreter.domain.file.CobolSequentialFile;
import io.proleap.cobol.interpreter.domain.file.impl.CobolSequentialFileImpl;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class ReadWriteStatementTest extends TestBase {

	@Disabled
	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/procedure/read/ReadWriteStatement.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();
		final CobolInterpreterParams interpreterParams = createInterpreterParams(parserParams);

		{
			final FileControlEntry fileControlEntry = getFileControlEntry("PersonRecords", programUnit);
			final CobolSequentialFile file = new CobolSequentialFileImpl();
			interpreterParams.getState().putFile(fileControlEntry, file);

			file.getRecords().add("1234567                       ");
			file.getRecords().add("7654321                       ");
		}

		{
			final FileControlEntry fileControlEntry = getFileControlEntry("TransRecords", programUnit);
			final CobolSequentialFile file = new CobolSequentialFileImpl();
			interpreterParams.getState().putFile(fileControlEntry, file);

			file.getRecords().add("2345671                       ");
			file.getRecords().add("6543219                       ");
		}

		{
			final FileControlEntry fileControlEntry = getFileControlEntry("NewPersonRecords", programUnit);
			final CobolSequentialFile file = new CobolSequentialFileImpl();
			interpreterParams.getState().putFile(fileControlEntry, file);
		}

		final CobolState state = cobolInterpreterRunner.run(programUnit, interpreterParams);

		assertEquals(4, state.getConsole().getLines().size());
		assertEquals("1234567                       ", state.getConsole().getLines().get(0));
		assertEquals("2345671                       ", state.getConsole().getLines().get(1));
		assertEquals("6543219                       ", state.getConsole().getLines().get(2));
		assertEquals("7654321                       ", state.getConsole().getLines().get(3));
	}
}
