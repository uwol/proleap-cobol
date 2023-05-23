package io.proleap.cobol.commons.value.domain.impl;

import io.proleap.cobol.commons.value.domain.CobolHighValue;

public class CobolHighValueImpl implements CobolHighValue {

	@Override
	public boolean equals(final Object o) {
		return o instanceof CobolHighValue;
	}

	@Override
	public CobolValueType getType() {
		return CobolValueType.HIGH_VALUE;
	}

	@Override
	public String toString() {
		return "HIGH-VALUE";
	}
}
