package io.proleap.cobol.commons.value.domain;

public interface CobolValue {

	enum CobolValueType {
		BOOLEAN, DECIMAL, HIGH_VALUE, LOW_VALUE, STRING
	}

	CobolValueType getType();
}
