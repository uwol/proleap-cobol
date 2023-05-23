package io.proleap.cobol.commons.value.domain.impl;

import io.proleap.cobol.commons.value.domain.CobolLowValue;

public class CobolLowValueImpl implements CobolLowValue {

	@Override
	public boolean equals(final Object o) {
		return o instanceof CobolLowValue;
	}

	@Override
	public CobolValueType getType() {
		return CobolValueType.LOW_VALUE;
	}

	@Override
	public String toString() {
		return "LOW-VALUE";
	}
}
