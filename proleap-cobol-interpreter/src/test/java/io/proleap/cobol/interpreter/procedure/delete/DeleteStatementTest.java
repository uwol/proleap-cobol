package io.proleap.cobol.interpreter.procedure.delete;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
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
public class DeleteStatementTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/procedure/delete/DeleteStatement.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();
		final CobolInterpreterParams interpreterParams = createInterpreterParams(parserParams);

		final FileControlEntry fileControlEntry = getFileControlEntry("PersonFile", programUnit);
		final CobolSequentialFile file = new CobolSequentialFileImpl();
		interpreterParams.getState().putFile(fileControlEntry, file);

		file.getRecords().add("1234567Hopper  GH19061209");
		file.getRecords().add("7654321Obama   BO19610804");

		final CobolState state = cobolInterpreterRunner.run(programUnit, interpreterParams);

		assertEquals(0, state.getConsole().getLines().size());

		assertEquals(1, file.getRecords().size());
		assertEquals("7654321Obama   BO19610804", file.getRecords().get(0));
	}
}
