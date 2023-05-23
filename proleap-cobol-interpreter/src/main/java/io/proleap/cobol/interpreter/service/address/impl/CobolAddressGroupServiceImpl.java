package io.proleap.cobol.interpreter.service.address.impl;

import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Singleton;

import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;
import io.proleap.cobol.interpreter.domain.address.impl.CobolAddressGroupImpl;
import io.proleap.cobol.interpreter.domain.address.impl.CobolAddressImpl;
import io.proleap.cobol.interpreter.service.address.CobolAddressGroupService;

@Singleton
public class CobolAddressGroupServiceImpl implements CobolAddressGroupService {

	protected CobolAddress createAddress(final CobolValue value, final Integer length) {
		final CobolAddress result = new CobolAddressImpl();
		result.setValue(value);
		result.setLength(length);
		return result;
	}

	@Override
	public CobolAddressGroup createAddressGroup() {
		return new CobolAddressGroupImpl();
	}

	@Override
	public CobolAddressGroup createAddressGroup(final CobolValue value, final Integer length) {
		final CobolAddress address = createAddress(value, length);

		final CobolAddressGroup result = createAddressGroup();
		result.getAddresses().add(address);

		return result;
	}

	@Override
	public CobolAddressGroup createAddressGroup(final List<CobolValue> values, final List<CobolAddress> addresses) {
		final CobolAddressGroup result = createAddressGroup();

		for (int i = 0; i < values.size(); i++) {
			final CobolValue value = values.get(i);
			final CobolAddress address = addresses.get(i);

			result.getAddresses().add(createAddress(value, address.getLength()));
		}

		return result;
	}

	@Override
	public List<CobolAddress> getAddresses(final List<CobolAddressGroup> addressGroups) {
		return addressGroups.stream().flatMap(addressGroup -> {
			return addressGroup.getAddresses().stream();
		}).collect(Collectors.toList());
	}
}
