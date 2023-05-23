package io.proleap.cobol.commons.value.domain.impl;

import java.math.BigDecimal;

import io.proleap.cobol.commons.value.domain.CobolDecimalValue;

public class CobolDecimalValueImpl implements CobolDecimalValue {

	public static CobolDecimalValue of(final BigDecimal value) {
		final CobolDecimalValueImpl result = new CobolDecimalValueImpl();
		result.setDecimal(value);
		return result;
	}

	protected BigDecimal decimal;

	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		}

		if (o == null) {
			return false;
		}

		if (!(o instanceof CobolDecimalValue)) {
			return false;
		}

		final CobolDecimalValue decimalValue = (CobolDecimalValue) o;
		return decimal.equals(decimalValue.getDecimal());
	}

	@Override
	public BigDecimal getDecimal() {
		return decimal;
	}

	@Override
	public CobolValueType getType() {
		return CobolValueType.DECIMAL;
	}

	public void setDecimal(final BigDecimal decimal) {
		this.decimal = decimal;
	}

	@Override
	public String toString() {
		return String.valueOf(decimal);
	}
}
