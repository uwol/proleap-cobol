package io.proleap.cobol.commons.value.domain.impl;

import io.proleap.cobol.commons.value.domain.CobolBooleanValue;

public class CobolBooleanValueImpl implements CobolBooleanValue {

	public static CobolBooleanValue FALSE = CobolBooleanValueImpl.of(false);

	public static CobolBooleanValue TRUE = CobolBooleanValueImpl.of(true);

	public static CobolBooleanValue of(final Boolean value) {
		final CobolBooleanValueImpl result = new CobolBooleanValueImpl();
		result.setBoolean(value);
		return result;
	}

	protected Boolean bool;

	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		}

		if (o == null) {
			return false;
		}

		if (!(o instanceof CobolBooleanValue)) {
			return false;
		}

		final CobolBooleanValue booleanValue = (CobolBooleanValue) o;
		return bool.equals(booleanValue.getBoolean());
	}

	@Override
	public Boolean getBoolean() {
		return bool;
	}

	@Override
	public CobolValueType getType() {
		return CobolValueType.BOOLEAN;
	}

	public void setBoolean(final Boolean bool) {
		this.bool = bool;
	}

	@Override
	public String toString() {
		return String.valueOf(bool);
	}
}
