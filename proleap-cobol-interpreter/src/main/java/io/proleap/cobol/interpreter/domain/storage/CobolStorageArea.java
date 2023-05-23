package io.proleap.cobol.interpreter.domain.storage;

import java.util.List;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.commons.value.CobolValueProvider;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;

public interface CobolStorageArea extends CobolValueProvider {

	CobolAddressGroup getAddressGroup(DataDescriptionEntry dataDescriptionEntry);

	List<DataDescriptionEntry> getKeys();

	void putAddressGroup(DataDescriptionEntry dataDescriptionEntry, CobolAddressGroup addressGroup);
}
