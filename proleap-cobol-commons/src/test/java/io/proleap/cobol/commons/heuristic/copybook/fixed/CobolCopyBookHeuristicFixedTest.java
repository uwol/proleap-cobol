package io.proleap.cobol.commons.heuristic.copybook.fixed;

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
public class CobolCopyBookHeuristicFixedTest extends TestBase {

	@Test
	public void testCopyBook() throws Exception {
		final File file = new File("src/test/resources/io/proleap/cobol/commons/heuristic/copybook/fixed/CopyBook.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		assertTrue(new CobolCopyBookHeuristicImpl().determineIsCopyBook(cobolText));
	}

	@Test
	public void testFixed() throws Exception {
		final File file = new File("src/test/resources/io/proleap/cobol/commons/heuristic/copybook/fixed/Fixed.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		assertFalse(new CobolCopyBookHeuristicImpl().determineIsCopyBook(cobolText));
	}

	@Test
	public void testFixedDeformedLines() throws Exception {
		final File file = new File(
				"src/test/resources/io/proleap/cobol/commons/heuristic/copybook/fixed/DeformedLines.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		assertFalse(new CobolCopyBookHeuristicImpl().determineIsCopyBook(cobolText));
	}

	@Test
	public void testFixedLargeIdentification() throws Exception {
		final File file = new File(
				"src/test/resources/io/proleap/cobol/commons/heuristic/copybook/fixed/FixedLargeIdentification.cbl");
		final String cobolText = Files.readString(file.toPath(), StandardCharsets.UTF_8);
		assertFalse(new CobolCopyBookHeuristicImpl().determineIsCopyBook(cobolText));
	}
}
