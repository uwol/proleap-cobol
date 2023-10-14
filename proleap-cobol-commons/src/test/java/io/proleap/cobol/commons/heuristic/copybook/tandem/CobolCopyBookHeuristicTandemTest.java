package io.proleap.cobol.commons.heuristic.copybook.tandem;

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
public class CobolCopyBookHeuristicTandemTest extends TestBase {

	@Test
	public void testCopyBook() throws Exception {
		final File file = new File(
				"src/test/resources/io/proleap/cobol/commons/heuristic/copybook/tandem/CopyBook.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		assertTrue(new CobolCopyBookHeuristicImpl().determineIsCopyBook(cobolText));
	}

	@Test
	public void testTandem() throws Exception {
		final File file = new File("src/test/resources/io/proleap/cobol/commons/heuristic/copybook/tandem/Tandem.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		assertFalse(new CobolCopyBookHeuristicImpl().determineIsCopyBook(cobolText));
	}

	@Test
	public void testTandemEmptyLines() throws Exception {
		final File file = new File(
				"src/test/resources/io/proleap/cobol/commons/heuristic/copybook/tandem/EmptyLines.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		assertFalse(new CobolCopyBookHeuristicImpl().determineIsCopyBook(cobolText));
	}

	@Test
	public void testTandemLargeIdentification() throws Exception {
		final File file = new File(
				"src/test/resources/io/proleap/cobol/commons/heuristic/copybook/tandem/TandemLargeIdentification.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		assertFalse(new CobolCopyBookHeuristicImpl().determineIsCopyBook(cobolText));
	}
}
