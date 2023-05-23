package io.proleap.cobol.interpreter.domain.address.impl;

import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;

public class CobolAddressImpl implements CobolAddress {

	private Integer length;

	private CobolValue value;

	@Override
	public Integer getLength() {
		return length;
	}

	@Override
	public CobolValue getValue() {
		return value;
	}

	@Override
	public void setLength(final Integer length) {
		this.length = length;
	}

	@Override
	public void setValue(final CobolValue value) {
		this.value = value;
	}

	@Override
	public String toString() {
		return String.valueOf(value) + " [" + String.valueOf(length) + "]";
	}
}
