package io.proleap.cobol.commons.value;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.commons.value.domain.CobolValue;

public interface CobolValueComparatorService {

	Integer compare(CobolValue value1, CobolValue value2, ProgramUnit programUnit);

	Boolean equal(CobolValue value1, CobolValue value2, ProgramUnit programUnit);

	Boolean greater(CobolValue value1, CobolValue value2, ProgramUnit programUnit);

	Boolean greaterOrEqual(CobolValue value1, CobolValue value2, ProgramUnit programUnit);

	Boolean less(CobolValue value1, CobolValue value2, ProgramUnit programUnit);

	Boolean lessOrEqual(CobolValue value1, CobolValue value2, ProgramUnit programUnit);

	Boolean notEqual(CobolValue value1, CobolValue value2, ProgramUnit programUnit);
}
