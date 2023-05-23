package io.proleap.cobol.interpreter.service.address;

import java.util.List;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.storage.CobolStorageArea;

public interface CobolFileControlEntryAddressService {

	List<CobolAddress> getAddresses(FileControlEntry fileControlEntry, CobolStorageArea storage);

	List<DataDescriptionEntry> getDataDescriptionEntries(FileControlEntry fileControlEntry);
}
