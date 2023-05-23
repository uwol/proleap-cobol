package io.proleap.cobol.interpreter.service.address.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.util.StringUtils;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.impl.CobolHighValueImpl;
import io.proleap.cobol.commons.value.domain.impl.CobolLowValueImpl;
import io.proleap.cobol.commons.value.domain.impl.CobolStringValueImpl;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.service.address.CobolAddressService;

@Singleton
public class CobolAddressServiceImpl implements CobolAddressService {

	@Inject
	private CobolValueService valueService;

	@Override
	public CobolValue mergeValues(final List<CobolValue> values, final ProgramUnit programUnit) {
		final CobolValue result;

		if (values.isEmpty()) {
			result = CobolStringValueImpl.of("");
		} else {
			final CobolValue firstValue = values.get(0);

			switch (firstValue.getType()) {
			case HIGH_VALUE:
				result = new CobolHighValueImpl();
				break;
			case LOW_VALUE:
				result = new CobolLowValueImpl();
				break;
			case BOOLEAN:
			case DECIMAL:
			case STRING:
			default:
				final String joined = values.stream().map(childValue -> {
					return valueService.getAsString(childValue, programUnit);
				}).collect(Collectors.joining());

				result = CobolStringValueImpl.of(joined);
				break;
			}
		}

		return result;
	}

	@Override
	public List<CobolValue> splitValue(final CobolValue value, final List<CobolAddress> addresses,
			final ProgramUnit programUnit) {
		final List<CobolValue> result;

		switch (value.getType()) {
		case HIGH_VALUE:
			result = addresses.stream().map(action -> {
				return new CobolHighValueImpl();
			}).collect(Collectors.toList());
			break;
		case LOW_VALUE:
			result = addresses.stream().map(action -> {
				return new CobolLowValueImpl();
			}).collect(Collectors.toList());
			break;
		case BOOLEAN:
		case DECIMAL:
		case STRING:
		default:
			final String valueAsString = valueService.getAsString(value, programUnit);
			final int valueLength = valueAsString.length();

			int start = 0;
			result = new ArrayList<>();

			for (int i = 0; i < addresses.size(); i++) {
				final CobolAddress address = addresses.get(i);
				final Integer addressLength = address.getLength();
				final int effectiveLength = addressLength == null ? valueLength - start
						: Math.min(valueLength - start, addressLength);
				final int end = start + effectiveLength;
				final String substring = valueAsString.substring(start, end);
				final String substringPadded = addressLength == null ? substring
						: StringUtils.rightPad(substring, addressLength);

				result.add(CobolStringValueImpl.of(substringPadded));
				start += effectiveLength;
			}

			break;
		}

		return result;
	}
}
