package io.proleap.cobol.commons.value;

import java.math.BigDecimal;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.commons.value.domain.CobolValue;

/**
 * Service for reading and writing CobolValue.
 */
public interface CobolValueService {

	BigDecimal getAsDecimal(CobolValue cobolValue);

	String getAsString(CobolValue cobolValue, ProgramUnit programUnit);

	Boolean getBoolean(CobolValue cobolValue);

	BigDecimal getDecimal(CobolValue cobolValue);

	String getString(CobolValue cobolValue);

	Object getValue(CobolValue cobolValue);
}
