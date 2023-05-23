package io.proleap.cobol.commons.value.domain;

import java.math.BigDecimal;

public interface CobolDecimalValue extends CobolValue {

	BigDecimal getDecimal();
}