package io.proleap.cobol.interpreter.service.address;

import java.util.List;

import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;

public interface CobolAddressGroupService {

	CobolAddressGroup createAddressGroup();

	CobolAddressGroup createAddressGroup(CobolValue value, Integer length);

	CobolAddressGroup createAddressGroup(List<CobolValue> values, List<CobolAddress> addresses);

	List<CobolAddress> getAddresses(List<CobolAddressGroup> addressGroups);
}
