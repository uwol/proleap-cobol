package io.proleap.cobol.interpreter.procedure.returnstmt;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
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
import io.proleap.cobol.interpreter.domain.file.impl.CobolSequentialFileImpl;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class ReturnStatementTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/procedure/returnstmt/ReturnStatement.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();
		final CobolInterpreterParams interpreterParams = createInterpreterParams(parserParams);

		{
			final FileControlEntry fileControlEntry = getFileControlEntry("SalesFile", programUnit);
			final CobolSequentialFile file = new CobolSequentialFileImpl();
			interpreterParams.getState().putFile(fileControlEntry, file);

			file.getRecords().add("1234 3");
			file.getRecords().add("4321 2");
			file.getRecords().add("1111 1001");
			file.getRecords().add("1234 2");
			file.getRecords().add("4321 1");
			file.getRecords().add("1111 1000");
		}

		final CobolState state = cobolInterpreterRunner.run(programUnit, interpreterParams);
		assertEquals(0, state.getConsole().getLines().size());

		{
			final FileControlEntry fileControlEntry = getFileControlEntry("WorkFile", programUnit);
			final CobolSequentialFile file = (CobolSequentialFile) state.getFile(fileControlEntry);
			final List<String> records = file.getRecords();

			assertEquals(6, records.size());
			assertEquals("1111 1001", records.get(0));
			assertEquals("1111 1000", records.get(1));
			assertEquals("1234 3   ", records.get(2));
			assertEquals("1234 2   ", records.get(3));
			assertEquals("4321 2   ", records.get(4));
			assertEquals("4321 1   ", records.get(5));
		}

		{
			final FileControlEntry fileControlEntry = getFileControlEntry("SalesSummaryFile", programUnit);
			final CobolSequentialFile file = (CobolSequentialFile) state.getFile(fileControlEntry);
			final List<String> records = file.getRecords();

			assertEquals(3, records.size());
			assertEquals("1111 2001", records.get(0));
			assertEquals("1234 5", records.get(1));
			assertEquals("4321 3", records.get(2));
		}
	}
}
