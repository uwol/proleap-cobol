package io.proleap.cobol.interpreter.service.address;

import java.util.List;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;

public interface CobolAddressService {

	CobolValue mergeValues(List<CobolValue> values, ProgramUnit programUnit);

	List<CobolValue> splitValue(CobolValue value, List<CobolAddress> addresses, ProgramUnit programUnit);
}
