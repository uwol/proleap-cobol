package io.proleap.cobol.interpreter.procedure.sort;

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
public class SortAndFilterStatementTest extends TestBase {

	@Test
	public void test() throws Exception {
		final File inputFile = new File(
				"src/test/resources/io/proleap/cobol/interpreter/procedure/sort/SortAndFilterStatement.cbl");
		final CobolSourceFormatEnum format = CobolSourceFormatEnum.TANDEM;
		final CobolParserParams parserParams = createParserParams(format, inputFile);

		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, format);
		final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();
		final FileControlEntry fileControlEntry = getFileControlEntry("PersonFile", programUnit);

		final CobolInterpreterParams interpreterParams = createInterpreterParams(parserParams);

		final CobolSequentialFile file = new CobolSequentialFileImpl();
		interpreterParams.getState().putFile(fileControlEntry, file);

		file.getRecords().add("1234567Hopper    GH19061209  F");
		file.getRecords().add("7654321Obama     BO19610804  M");
		file.getRecords().add("6543219Fisher    IF18670227  M");
		file.getRecords().add("5432198Curie     MC18671107  F");

		final CobolState state = cobolInterpreterRunner.run(programUnit, interpreterParams);
		assertEquals(0, state.getConsole().getLines().size());

		final FileControlEntry malePersonFileControlEntry = getFileControlEntry("MalePersonFile", programUnit);
		final CobolSequentialFile malePersonFile = (CobolSequentialFile) state.getFile(malePersonFileControlEntry);
		final List<String> maleRecords = malePersonFile.getRecords();

		assertEquals(2, maleRecords.size());
		assertEquals("6543219Fisher    IF18670227  M", maleRecords.get(0));
		assertEquals("7654321Obama     BO19610804  M", maleRecords.get(1));
	}
}
