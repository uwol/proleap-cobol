package io.proleap.cobol.interpreter.service.storage;

import java.util.List;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.commons.datadescription.CobolValueProviderService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;
import io.proleap.cobol.interpreter.domain.storage.CobolStorageArea;

public interface CobolStorageService extends CobolValueProviderService {

	CobolAddressGroup getAddressGroup(DataDescriptionEntry dataDescriptionEntry, CobolStorageArea storage);

	List<CobolAddressGroup> getAddressGroups(List<DataDescriptionEntry> dataDescriptionEntries,
			CobolStorageArea storage);

	void putAddressGroup(DataDescriptionEntry dataDescriptionEntry, CobolAddressGroup addressGroup,
			CobolStorageArea storage);

	void putValue(DataDescriptionEntry dataDescriptionEntry, CobolValue value, CobolStorageArea storage);

	void putValue(List<DataDescriptionEntry> dataDescriptionEntries, CobolValue value, CobolStorageArea storage);
}
