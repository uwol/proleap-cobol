package io.proleap.cobol.interpreter.domain.storage.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;
import io.proleap.cobol.interpreter.domain.storage.CobolStorageArea;
import io.proleap.cobol.interpreter.exception.CobolInterpreterException;

public class CobolStorageAreaImpl implements CobolStorageArea {

	protected final Map<DataDescriptionEntry, CobolAddressGroup> addressGroups = new HashMap<>();

	@Override
	public CobolAddressGroup getAddressGroup(final DataDescriptionEntry dataDescriptionEntry) {
		return addressGroups.get(dataDescriptionEntry);
	}

	@Override
	public List<DataDescriptionEntry> getKeys() {
		return new ArrayList<>(addressGroups.keySet());
	}

	@Override
	public List<CobolValue> getValues(final DataDescriptionEntry dataDescriptionEntry) {
		final CobolAddressGroup addressGroup = getAddressGroup(dataDescriptionEntry);
		final List<CobolValue> result;

		if (addressGroup == null) {
			throw new CobolInterpreterException("Missing memory address group for " + dataDescriptionEntry);
		} else {
			result = addressGroup.getAddresses().stream().map(address -> {
				return address.getValue();
			}).collect(Collectors.toList());
		}

		return result;
	}

	@Override
	public void putAddressGroup(final DataDescriptionEntry dataDescriptionEntry, final CobolAddressGroup addressGroup) {
		addressGroups.put(dataDescriptionEntry, addressGroup);
	}
}
