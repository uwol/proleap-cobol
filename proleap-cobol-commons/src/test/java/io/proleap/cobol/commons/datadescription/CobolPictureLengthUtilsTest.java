package io.proleap.cobol.commons.datadescription;

import static org.junit.jupiter.api.Assertions.assertEquals;

import jakarta.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import io.proleap.cobol.commons.TestBase;

@MicronautTest
public class CobolPictureLengthUtilsTest extends TestBase {

	@Inject
	private CobolPictureLengthService cobolPictureLengthService;

	@Test
	public void testGetFractionalPartLength9_9() throws Exception {
		assertEquals(Integer.valueOf(1), cobolPictureLengthService.getFractionalPartLength("9V9"));
	}

	@Test
	public void testGetFractionalPartLength9_99() throws Exception {
		assertEquals(Integer.valueOf(2), cobolPictureLengthService.getFractionalPartLength("9V99"));
	}

	@Test
	public void testGetFractionalPartLength9_999() throws Exception {
		assertEquals(Integer.valueOf(3), cobolPictureLengthService.getFractionalPartLength("9V999"));
	}

	@Test
	public void testGetFractionalPartLength9DOT9() throws Exception {
		assertEquals(Integer.valueOf(1), cobolPictureLengthService.getFractionalPartLength("9.9"));
	}

	@Test
	public void testGetFractionalPartLength9DOT99() throws Exception {
		assertEquals(Integer.valueOf(2), cobolPictureLengthService.getFractionalPartLength("9.99"));
	}

	@Test
	public void testGetIntegerPartLength9_01() throws Exception {
		assertEquals(Integer.valueOf(1), cobolPictureLengthService.getIntegerPartLength("9(01)"));
	}

	@Test
	public void testGetIntegerPartLength9_02() throws Exception {
		assertEquals(Integer.valueOf(2), cobolPictureLengthService.getIntegerPartLength("9(02)"));
	}

	@Test
	public void testGetIntegerPartLength9_1() throws Exception {
		assertEquals(Integer.valueOf(1), cobolPictureLengthService.getIntegerPartLength("9(1)"));
	}

	@Test
	public void testGetIntegerPartLength9_2() throws Exception {
		assertEquals(Integer.valueOf(2), cobolPictureLengthService.getIntegerPartLength("9(2)"));
	}

	@Test
	public void testGetIntegerPartLength9_9() throws Exception {
		assertEquals(Integer.valueOf(1), cobolPictureLengthService.getIntegerPartLength("9V9"));
	}

	@Test
	public void testGetIntegerPartLength99_9() throws Exception {
		assertEquals(Integer.valueOf(2), cobolPictureLengthService.getIntegerPartLength("99V9"));
	}

	@Test
	public void testGetIntegerPartLength999_9() throws Exception {
		assertEquals(Integer.valueOf(3), cobolPictureLengthService.getIntegerPartLength("999V9"));
	}

	@Test
	public void testGetStringLengthX_01() throws Exception {
		assertEquals(Integer.valueOf(1), cobolPictureLengthService.getStringLength("X(01)"));
	}

	@Test
	public void testGetStringLengthX_02() throws Exception {
		assertEquals(Integer.valueOf(2), cobolPictureLengthService.getStringLength("X(02)"));
	}

	@Test
	public void testGetStringLengthX_1() throws Exception {
		assertEquals(Integer.valueOf(1), cobolPictureLengthService.getStringLength("X(1)"));
	}

	@Test
	public void testGetStringLengthX_2() throws Exception {
		assertEquals(Integer.valueOf(2), cobolPictureLengthService.getStringLength("X(2)"));
	}
}
