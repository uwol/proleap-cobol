package io.proleap.cobol.interpreter.procedure.rewrite;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.util.List;

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
public class RewriteStatementTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/procedure/rewrite/RewriteStatement.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();
		final CobolInterpreterParams interpreterParams = createInterpreterParams(parserParams);

		final FileControlEntry fileControlEntry = getFileControlEntry("PERSON", programUnit);
		final CobolSequentialFile file = new CobolSequentialFileImpl();
		interpreterParams.getState().putFile(fileControlEntry, file);

		file.getRecords().add("1234Hopper      F");
		file.getRecords().add("2345Obama       M");
		file.getRecords().add("3456Fisher      M");
		file.getRecords().add("4567Curie       F");

		final CobolState state = cobolInterpreterRunner.run(programUnit, interpreterParams);
		assertEquals(0, state.getConsole().getLines().size());

		final List<String> records = file.getRecords();

		assertEquals(4, records.size());
		assertEquals("1234Hopper      F", records.get(0));
		assertEquals("2345Bush        M", records.get(1));
		assertEquals("3456Fisher      M", records.get(2));
		assertEquals("4567Curie       F", records.get(3));
	}
}
