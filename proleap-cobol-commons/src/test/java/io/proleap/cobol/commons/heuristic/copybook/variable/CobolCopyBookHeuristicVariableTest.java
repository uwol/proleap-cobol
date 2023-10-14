package io.proleap.cobol.commons.heuristic.copybook.variable;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

import org.junit.jupiter.api.Test;

import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import io.proleap.cobol.commons.TestBase;
import io.proleap.cobol.commons.heuristic.copy.impl.CobolCopyBookHeuristicImpl;

@MicronautTest
public class CobolCopyBookHeuristicVariableTest extends TestBase {

	@Test
	public void testCopyBook() throws Exception {
		final File file = new File(
				"src/test/resources/io/proleap/cobol/commons/heuristic/copybook/variable/CopyBook.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		assertTrue(new CobolCopyBookHeuristicImpl().determineIsCopyBook(cobolText));
	}

	@Test
	public void testVariable() throws Exception {
		final File file = new File(
				"src/test/resources/io/proleap/cobol/commons/heuristic/copybook/variable/Variable.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		assertFalse(new CobolCopyBookHeuristicImpl().determineIsCopyBook(cobolText));
	}

	@Test
	public void testVariableEmptyLines() throws Exception {
		final File file = new File(
				"src/test/resources/io/proleap/cobol/commons/heuristic/copybook/variable/EmptyLines.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		assertFalse(new CobolCopyBookHeuristicImpl().determineIsCopyBook(cobolText));
	}

	@Test
	public void testVariableLargeIdentification() throws Exception {
		final File file = new File(
				"src/test/resources/io/proleap/cobol/commons/heuristic/copybook/variable/VariableLargeIdentification.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		assertFalse(new CobolCopyBookHeuristicImpl().determineIsCopyBook(cobolText));
	}
}
