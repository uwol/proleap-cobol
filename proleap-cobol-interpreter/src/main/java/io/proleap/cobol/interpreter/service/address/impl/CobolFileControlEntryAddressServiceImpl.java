package io.proleap.cobol.interpreter.service.address.impl;

import java.util.List;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;
import io.proleap.cobol.interpreter.domain.storage.CobolStorageArea;
import io.proleap.cobol.interpreter.service.address.CobolAddressGroupService;
import io.proleap.cobol.interpreter.service.address.CobolFileControlEntryAddressService;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class CobolFileControlEntryAddressServiceImpl implements CobolFileControlEntryAddressService {

	@Inject
	private CobolAddressGroupService addressGroupService;

	@Inject
	private CobolStorageService storageService;

	@Override
	public List<CobolAddress> getAddresses(final FileControlEntry fileControlEntry, final CobolStorageArea storage) {
		final List<DataDescriptionEntry> fileDataDescriptionEntries = getDataDescriptionEntries(fileControlEntry);
		final List<CobolAddressGroup> addressGroups = storageService.getAddressGroups(fileDataDescriptionEntries,
				storage);
		return addressGroupService.getAddresses(addressGroups);
	}

	@Override
	public List<DataDescriptionEntry> getDataDescriptionEntries(final FileControlEntry fileControlEntry) {
		final FileDescriptionEntry fileDescriptionEntry = fileControlEntry.getFileDescriptionEntry();
		return fileDescriptionEntry.getRootDataDescriptionEntries();
	}
}
