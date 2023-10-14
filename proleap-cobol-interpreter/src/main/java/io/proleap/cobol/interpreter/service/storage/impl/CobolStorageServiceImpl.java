package io.proleap.cobol.interpreter.service.storage.impl;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.commons.value.CobolValueProvider;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;
import io.proleap.cobol.interpreter.domain.storage.CobolStorageArea;
import io.proleap.cobol.interpreter.service.address.CobolAddressGroupService;
import io.proleap.cobol.interpreter.service.address.CobolAddressService;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class CobolStorageServiceImpl implements CobolStorageService {

	@Inject
	private CobolAddressGroupService addressGroupService;

	@Inject
	private CobolAddressService addressService;

	@Override
	public CobolAddressGroup getAddressGroup(final DataDescriptionEntry dataDescriptionEntry,
			final CobolStorageArea storage) {
		return storage.getAddressGroup(dataDescriptionEntry);
	}

	@Override
	public List<CobolAddressGroup> getAddressGroups(final List<DataDescriptionEntry> dataDescriptionEntries,
			final CobolStorageArea storage) {
		return dataDescriptionEntries.stream().map(dataDescriptionEntry -> {
			return getAddressGroup(dataDescriptionEntry, storage);
		}).collect(Collectors.toList());
	}

	@Override
	public CobolValue getValue(final DataDescriptionEntry dataDescriptionEntry,
			final CobolValueProvider valueProvider) {
		final CobolValue result;

		if (valueProvider == null) {
			result = null;
		} else {
			final List<CobolValue> values = valueProvider.getValues(dataDescriptionEntry);

			if (values == null) {
				result = null;
			} else if (values.size() == 1) {
				result = values.get(0);
			} else {
				final ProgramUnit programUnit = dataDescriptionEntry.getProgramUnit();
				result = addressService.mergeValues(values, programUnit);
			}
		}

		return result;
	}

	@Override
	public void putAddressGroup(final DataDescriptionEntry dataDescriptionEntry, final CobolAddressGroup addressGroup,
			final CobolStorageArea storage) {
		storage.putAddressGroup(dataDescriptionEntry, addressGroup);
	}

	@Override
	public void putValue(final DataDescriptionEntry dataDescriptionEntry, final CobolValue value,
			final CobolStorageArea storage) {
		putValue(Arrays.asList(dataDescriptionEntry), value, storage);
	}

	@Override
	public void putValue(final List<DataDescriptionEntry> dataDescriptionEntries, final CobolValue value,
			final CobolStorageArea storage) {
		final List<CobolAddressGroup> addressGroups = getAddressGroups(dataDescriptionEntries, storage);

		if (value == null) {
		} else if (addressGroups.isEmpty() || addressGroups.get(0) == null) {
		} else {
			final List<CobolAddress> addresses = addressGroupService.getAddresses(addressGroups);
			addresses.forEach(address -> address.setValue(null));

			final CobolAddress firstAddress = addresses.get(0);

			switch (value.getType()) {
			case HIGH_VALUE:
			case LOW_VALUE:
				addresses.forEach(address -> address.setValue(value));
				break;
			case STRING:
				final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntries.get(0);
				final ProgramUnit programUnit = dataDescriptionEntry.getProgramUnit();
				final List<CobolValue> addressValues = addressService.splitValue(value, addresses, programUnit);

				for (int i = 0; i < addresses.size(); i++) {
					final CobolAddress address = addresses.get(i);
					final CobolValue addressValue = addressValues.get(i);
					address.setValue(addressValue);
				}
				break;
			case BOOLEAN:
			case DECIMAL:
			default:
				firstAddress.setValue(value);
				break;
			}
		}
	}
}
