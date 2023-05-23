package io.proleap.cobol.commons.value;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.value.domain.CobolValue;

/**
 * Service for resolving value statements.
 */
public interface CobolValueStmtService {

	final static String WS = " ";

	CobolValue getValue(ValueStmt valueStmt, CobolValueProvider valueProvider);

	// FIXME return List<CobolValue> as conditions can have multiple values
	CobolValue getValueClauseValue(DataDescriptionEntry dataDescriptionEntry, CobolValueProvider valueProvider);
}
