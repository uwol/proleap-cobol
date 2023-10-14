package io.proleap.cobol.interpreter.procedure.write;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.interpreter.TestBase;
import io.proleap.cobol.interpreter.domain.file.CobolSequentialFile;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class WriteStatementTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/procedure/write/WriteStatement.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();

		final CobolInterpreterParams interpreterParams = createInterpreterParams(parserParams);
		interpreterParams.setAcceptParams(Arrays.asList("1234567Hopper  GH19061209", "7654321Obama   BO19610804", " "));

		final CobolState state = cobolInterpreterRunner.run(programUnit, interpreterParams);

		assertEquals(7, state.getConsole().getLines().size());

		assertEquals("Enter person details using template below without data behind.",
				state.getConsole().getLines().get(0));

		assertEquals("Enter - PersonId, Lastname, Initials, YOB, MOB, DOB", state.getConsole().getLines().get(1));
		assertEquals("NNNNNNNPPPPPPPPIIYYYYMMDD", state.getConsole().getLines().get(2));

		assertEquals("Enter - PersonId, Lastname, Initials, YOB, MOB, DOB", state.getConsole().getLines().get(3));
		assertEquals("NNNNNNNPPPPPPPPIIYYYYMMDD", state.getConsole().getLines().get(4));

		assertEquals("Enter - PersonId, Lastname, Initials, YOB, MOB, DOB", state.getConsole().getLines().get(5));
		assertEquals("NNNNNNNPPPPPPPPIIYYYYMMDD", state.getConsole().getLines().get(6));

		final FileControlEntry fileControlEntry = getFileControlEntry("PersonFile", programUnit);
		final CobolSequentialFile file = (CobolSequentialFile) state.getFile(fileControlEntry);
		final List<String> records = file.getRecords();

		assertEquals(2, records.size());
		assertEquals("1234567Hopper  GH19061209", records.get(0));
		assertEquals("7654321Obama   BO19610804", records.get(1));
	}
}
