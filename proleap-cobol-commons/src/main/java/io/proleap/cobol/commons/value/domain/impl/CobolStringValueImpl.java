package io.proleap.cobol.commons.value.domain.impl;

import io.proleap.cobol.commons.value.domain.CobolStringValue;

public class CobolStringValueImpl implements CobolStringValue {

	public static CobolStringValue of(final String value) {
		final CobolStringValueImpl result = new CobolStringValueImpl();
		result.setString(value);
		return result;
	}

	protected String string;

	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		}

		if (o == null) {
			return false;
		}

		if (!(o instanceof CobolStringValue)) {
			return false;
		}

		final CobolStringValue stringValue = (CobolStringValue) o;
		return string.equals(stringValue.getString());
	}

	@Override
	public String getString() {
		return string;
	}

	@Override
	public CobolValueType getType() {
		return CobolValueType.STRING;
	}

	public void setString(final String string) {
		this.string = string;
	}

	@Override
	public String toString() {
		return string;
	}
}
