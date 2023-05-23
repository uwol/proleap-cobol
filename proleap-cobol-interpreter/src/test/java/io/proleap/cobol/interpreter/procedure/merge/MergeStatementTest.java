package io.proleap.cobol.interpreter.procedure.merge;

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
public class MergeStatementTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/procedure/merge/MergeStatement.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();
		final CobolInterpreterParams interpreterParams = createInterpreterParams(parserParams);

		{
			final FileControlEntry fileControlEntry = getFileControlEntry("PersonFile", programUnit);
			final CobolSequentialFile file = new CobolSequentialFileImpl();
			interpreterParams.getState().putFile(fileControlEntry, file);

			file.getRecords().add("1234567Hopper    GH19061209  F");
			file.getRecords().add("7654321Obama     BO19610804  M");
		}

		{
			final FileControlEntry fileControlEntry = getFileControlEntry("InsertionsFile", programUnit);
			final CobolSequentialFile file = new CobolSequentialFileImpl();
			interpreterParams.getState().putFile(fileControlEntry, file);

			file.getRecords().add("6543219Fisher    IF18670227  M");
			file.getRecords().add("5432198Curie     MC18671107  F");
		}

		final CobolState state = cobolInterpreterRunner.run(programUnit, interpreterParams);
		assertEquals(0, state.getConsole().getLines().size());

		final FileControlEntry fileControlEntry = getFileControlEntry("NewPersonFile", programUnit);
		final CobolSequentialFile file = (CobolSequentialFile) state.getFile(fileControlEntry);
		final List<String> records = file.getRecords();

		assertEquals(4, records.size());
		assertEquals("1234567Hopper    GH19061209  F", records.get(0));
		assertEquals("5432198Curie     MC18671107  F", records.get(1));
		assertEquals("6543219Fisher    IF18670227  M", records.get(2));
		assertEquals("7654321Obama     BO19610804  M", records.get(3));
	}
}
