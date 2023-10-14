package io.proleap.cobol.commons.heuristic.lineformat.variable;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

import org.junit.jupiter.api.Test;

import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import io.proleap.cobol.commons.TestBase;
import io.proleap.cobol.commons.heuristic.lineformat.impl.CobolLineFormatHeuristicImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

@MicronautTest
public class CobolLineFormatHeuristicVariableTest extends TestBase {

	@Test
	public void testVariable1() throws Exception {
		final File file = new File(
				"src/test/resources/io/proleap/cobol/commons/heuristic/lineformat/variable/Variable.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		final CobolSourceFormatEnum lineFormat = new CobolLineFormatHeuristicImpl().determineLineFormat(cobolText);
		assertEquals(CobolSourceFormatEnum.VARIABLE, lineFormat);
	}

	@Test
	public void testVariableEmptyLines() throws Exception {
		final File file = new File(
				"src/test/resources/io/proleap/cobol/commons/heuristic/lineformat/variable/EmptyLines.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		final CobolSourceFormatEnum lineFormat = new CobolLineFormatHeuristicImpl().determineLineFormat(cobolText);
		assertEquals(CobolSourceFormatEnum.VARIABLE, lineFormat);
	}

	@Test
	public void testVariableLargeIdentification() throws Exception {
		final File file = new File(
				"src/test/resources/io/proleap/cobol/commons/heuristic/lineformat/variable/VariableLargeIdentification.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		final CobolSourceFormatEnum lineFormat = new CobolLineFormatHeuristicImpl().determineLineFormat(cobolText);
		assertEquals(CobolSourceFormatEnum.VARIABLE, lineFormat);
	}
}
