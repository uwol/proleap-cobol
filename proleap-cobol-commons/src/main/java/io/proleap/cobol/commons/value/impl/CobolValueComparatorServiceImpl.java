package io.proleap.cobol.commons.value.impl;

import java.math.BigDecimal;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.commons.value.CobolValueComparatorService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.CobolValue.CobolValueType;

@Singleton
public class CobolValueComparatorServiceImpl implements CobolValueComparatorService {

	@Inject
	private CobolValueService valueService;

	@Override
	public Integer compare(final CobolValue valueLeft, final CobolValue valueRight, final ProgramUnit programUnit) {
		final Boolean booleanValueLeft = valueService.getBoolean(valueLeft);
		final Boolean booleanValueRight = valueService.getBoolean(valueRight);

		final BigDecimal decimalValueLeft = valueService.getDecimal(valueLeft);
		final BigDecimal decimalValueRight = valueService.getDecimal(valueRight);

		final String stringValueLeft = valueService.getAsString(valueLeft, programUnit).stripTrailing();
		final String stringValueRight = valueService.getAsString(valueRight, programUnit).stripTrailing();

		final Integer result;

		if (valueLeft != null && valueLeft.equals(valueRight)) {
			result = 0;
		} else if (booleanValueLeft != null && booleanValueRight != null) {
			result = booleanValueLeft.compareTo(booleanValueRight);
		} else if (decimalValueLeft != null && decimalValueRight != null) {
			result = decimalValueLeft.compareTo(decimalValueRight);
		} else if (stringValueLeft != null && stringValueRight != null) {
			result = stringValueLeft.compareTo(stringValueRight);
		} else {
			result = null;
		}

		return result;
	}

	@Override
	public Boolean equal(final CobolValue value1, final CobolValue value2, final ProgramUnit programUnit) {
		final CobolValueType type1 = value1.getType();
		final CobolValueType type2 = value2.getType();
		final Integer comparison = compare(value1, value2, programUnit);
		final Boolean result;

		if (CobolValueType.HIGH_VALUE.equals(type1) && CobolValueType.HIGH_VALUE.equals(type2)) {
			result = true;
		} else if (CobolValueType.LOW_VALUE.equals(type1) && CobolValueType.LOW_VALUE.equals(type2)) {
			result = true;
		} else if (CobolValueType.HIGH_VALUE.equals(type1) && CobolValueType.LOW_VALUE.equals(type2)) {
			result = false;
		} else if (CobolValueType.LOW_VALUE.equals(type1) && CobolValueType.HIGH_VALUE.equals(type2)) {
			result = false;
		} else if (comparison == null) {
			result = null;
		} else {
			result = comparison == 0;
		}

		return result;
	}

	@Override
	public Boolean greater(final CobolValue value1, final CobolValue value2, final ProgramUnit programUnit) {
		final Integer comparison = compare(value1, value2, programUnit);
		return comparison == null ? null : comparison > 0;
	}

	@Override
	public Boolean greaterOrEqual(final CobolValue value1, final CobolValue value2, final ProgramUnit programUnit) {
		final Integer comparison = compare(value1, value2, programUnit);
		return comparison == null ? null : comparison >= 0;
	}

	@Override
	public Boolean less(final CobolValue value1, final CobolValue value2, final ProgramUnit programUnit) {
		final Integer comparison = compare(value1, value2, programUnit);
		return comparison == null ? null : comparison < 0;
	}

	@Override
	public Boolean lessOrEqual(final CobolValue value1, final CobolValue value2, final ProgramUnit programUnit) {
		final Integer comparison = compare(value1, value2, programUnit);
		return comparison == null ? null : comparison <= 0;
	}

	@Override
	public Boolean notEqual(final CobolValue value1, final CobolValue value2, final ProgramUnit programUnit) {
		final CobolValueType type1 = value1.getType();
		final CobolValueType type2 = value2.getType();
		final Integer comparison = compare(value1, value2, programUnit);
		final Boolean result;

		if (CobolValueType.HIGH_VALUE.equals(type1) && !CobolValueType.HIGH_VALUE.equals(type2)) {
			result = true;
		} else if (!CobolValueType.HIGH_VALUE.equals(type1) && CobolValueType.HIGH_VALUE.equals(type2)) {
			result = true;
		} else if (CobolValueType.LOW_VALUE.equals(type1) && !CobolValueType.LOW_VALUE.equals(type2)) {
			result = true;
		} else if (!CobolValueType.LOW_VALUE.equals(type1) && CobolValueType.LOW_VALUE.equals(type2)) {
			result = true;
		} else if (comparison == null) {
			result = null;
		} else {
			result = comparison != 0;
		}

		return result;
	}
}
