package io.proleap.cobol.interpreter.service.address;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.inject.Inject;

import org.junit.jupiter.api.Test;

import io.micronaut.test.annotation.MicronautTest;
import io.proleap.cobol.commons.value.domain.CobolStringValue;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.CobolValue.CobolValueType;
import io.proleap.cobol.commons.value.domain.impl.CobolHighValueImpl;
import io.proleap.cobol.commons.value.domain.impl.CobolLowValueImpl;
import io.proleap.cobol.commons.value.domain.impl.CobolStringValueImpl;
import io.proleap.cobol.interpreter.TestBase;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.address.impl.CobolAddressImpl;

@MicronautTest
public class CobolAddressServiceTest extends TestBase {

	@Inject
	private CobolAddressService addressService;

	@Test
	public void testMergeValuesHighValueMultiple() throws Exception {
		final CobolValue result = addressService
				.mergeValues(Arrays.asList(new CobolHighValueImpl(), new CobolHighValueImpl()), null);
		assertEquals(CobolValueType.HIGH_VALUE, result.getType());
	}

	@Test
	public void testMergeValuesHighValueSingle() throws Exception {
		final CobolValue result = addressService.mergeValues(Arrays.asList(new CobolHighValueImpl()), null);
		assertEquals(CobolValueType.HIGH_VALUE, result.getType());
	}

	@Test
	public void testMergeValuesLowValueMultiple() throws Exception {
		final CobolValue result = addressService
				.mergeValues(Arrays.asList(new CobolLowValueImpl(), new CobolLowValueImpl()), null);
		assertEquals(CobolValueType.LOW_VALUE, result.getType());
	}

	@Test
	public void testMergeValuesLowValueSingle() throws Exception {
		final CobolValue result = addressService.mergeValues(Arrays.asList(new CobolLowValueImpl()), null);
		assertEquals(CobolValueType.LOW_VALUE, result.getType());
	}

	@Test
	public void testSplitValueHighValue() throws Exception {
		final CobolValue value = new CobolHighValueImpl();
		final List<CobolAddress> addresses = new ArrayList<>();

		{
			final CobolAddress cobolAddress = new CobolAddressImpl();
			cobolAddress.setLength(3);
			addresses.add(cobolAddress);
		}

		{
			final CobolAddress cobolAddress = new CobolAddressImpl();
			cobolAddress.setLength(3);
			addresses.add(cobolAddress);
		}

		{
			final CobolAddress cobolAddress = new CobolAddressImpl();
			cobolAddress.setLength(3);
			addresses.add(cobolAddress);
		}

		final List<CobolValue> splitValues = addressService.splitValue(value, addresses, null);
		assertEquals(3, splitValues.size());

		{
			final CobolValue splitValue = splitValues.get(0);
			assertEquals(CobolValueType.HIGH_VALUE, splitValue.getType());
		}

		{
			final CobolValue splitValue = splitValues.get(1);
			assertEquals(CobolValueType.HIGH_VALUE, splitValue.getType());
		}

		{
			final CobolValue splitValue = splitValues.get(2);
			assertEquals(CobolValueType.HIGH_VALUE, splitValue.getType());
		}
	}

	@Test
	public void testSplitValueProgramUnitNull() throws Exception {
		final CobolValue value = CobolStringValueImpl.of("12345678");
		final List<CobolAddress> addresses = new ArrayList<>();

		{
			final CobolAddress cobolAddress = new CobolAddressImpl();
			cobolAddress.setLength(2);
			addresses.add(cobolAddress);
		}

		{
			final CobolAddress cobolAddress = new CobolAddressImpl();
			cobolAddress.setLength(null);
			addresses.add(cobolAddress);
		}

		final List<CobolValue> splitValues = addressService.splitValue(value, addresses, null);
		assertEquals(2, splitValues.size());

		{
			final CobolValue splitValue = splitValues.get(0);
			final CobolStringValue splitStringValue = (CobolStringValue) splitValue;
			assertEquals("12", splitStringValue.getString());
		}

		{
			final CobolValue splitValue = splitValues.get(1);
			final CobolStringValue splitStringValue = (CobolStringValue) splitValue;
			assertEquals("345678", splitStringValue.getString());
		}
	}

	@Test
	public void testSplitValueString() throws Exception {
		final CobolValue value = CobolStringValueImpl.of("12345678");
		final List<CobolAddress> addresses = new ArrayList<>();

		{
			final CobolAddress cobolAddress = new CobolAddressImpl();
			cobolAddress.setLength(3);
			addresses.add(cobolAddress);
		}

		{
			final CobolAddress cobolAddress = new CobolAddressImpl();
			cobolAddress.setLength(3);
			addresses.add(cobolAddress);
		}

		{
			final CobolAddress cobolAddress = new CobolAddressImpl();
			cobolAddress.setLength(3);
			addresses.add(cobolAddress);
		}

		final List<CobolValue> splitValues = addressService.splitValue(value, addresses, null);
		assertEquals(3, splitValues.size());

		{
			final CobolValue splitValue = splitValues.get(0);
			final CobolStringValue splitStringValue = (CobolStringValue) splitValue;
			assertEquals("123", splitStringValue.getString());
		}

		{
			final CobolValue splitValue = splitValues.get(1);
			final CobolStringValue splitStringValue = (CobolStringValue) splitValue;
			assertEquals("456", splitStringValue.getString());
		}

		{
			final CobolValue splitValue = splitValues.get(2);
			final CobolStringValue splitStringValue = (CobolStringValue) splitValue;
			assertEquals("78 ", splitStringValue.getString());
		}
	}
}
