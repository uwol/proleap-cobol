package io.proleap.cobol.interpreter.domain.address;

import io.proleap.cobol.commons.value.domain.CobolValue;

public interface CobolAddress {

	Integer getLength();

	CobolValue getValue();

	void setLength(Integer length);

	void setValue(CobolValue value);
}
