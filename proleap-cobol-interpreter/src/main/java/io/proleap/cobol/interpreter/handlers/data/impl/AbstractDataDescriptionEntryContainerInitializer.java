package io.proleap.cobol.interpreter.handlers.data.impl;

import jakarta.inject.Inject;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryContainer;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryRename;
import io.proleap.cobol.asg.metamodel.data.datadescription.RenamesClause;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.datadescription.CobolPictureLengthService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;
import io.proleap.cobol.interpreter.domain.storage.CobolStorageArea;
import io.proleap.cobol.interpreter.handlers.impl.AbstractCobolInterpreterHandler;
import io.proleap.cobol.interpreter.service.address.CobolAddressGroupService;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

public abstract class AbstractDataDescriptionEntryContainerInitializer extends AbstractCobolInterpreterHandler {

	@Inject
	private CobolAddressGroupService addressGroupService;

	@Inject
	private CobolPictureLengthService cobolPictureLengthService;

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolStorageService storageService;

	@Inject
	private CobolValueStmtService valueStmtService;

	private CobolAddressGroup initDataDescriptionEntry(final DataDescriptionEntry dataDescriptionEntry,
			final CobolStorageArea memory) {
		final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry.getDataDescriptionEntryType();
		final CobolAddressGroup result;

		switch (dataDescriptionEntryType) {
		case SCALAR:
			result = initDataDescriptionEntryScalar(dataDescriptionEntry, memory);
			break;
		case GROUP:
			if (dataDescriptionEntryService.hasChildren(dataDescriptionEntry)) {
				result = initDataDescriptionEntryGroup(dataDescriptionEntry, memory);
			} else {
				result = initDataDescriptionEntryScalar(dataDescriptionEntry, memory);
			}
			break;
		case RENAME:
			final DataDescriptionEntryRename dataDescriptionEntryRename = (DataDescriptionEntryRename) dataDescriptionEntry;
			result = initDataDescriptionEntryRename(dataDescriptionEntryRename, memory);
			break;
		case EXEC_SQL:
		case CONDITION:
		default:
			result = null;
			break;
		}

		return result;
	}

	protected void initDataDescriptionEntryContainer(final DataDescriptionEntryContainer container,
			final CobolStorageArea memory) {
		for (final DataDescriptionEntry rootDataDescriptionEntry : container.getRootDataDescriptionEntries()) {
			initDataDescriptionEntry(rootDataDescriptionEntry, memory);
		}
	}

	private CobolAddressGroup initDataDescriptionEntryGroup(final DataDescriptionEntry dataDescriptionEntry,
			final CobolStorageArea memory) {
		final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
		final CobolAddressGroup result = addressGroupService.createAddressGroup();

		for (final DataDescriptionEntry subDataDescriptionEntry : dataDescriptionEntryGroup
				.getDataDescriptionEntries()) {
			final CobolAddressGroup subAddressGroup = initDataDescriptionEntry(subDataDescriptionEntry, memory);

			if (subAddressGroup != null) {
				result.getAddresses().addAll(subAddressGroup.getAddresses());
			}
		}

		storageService.putAddressGroup(dataDescriptionEntry, result, memory);

		return result;
	}

	private CobolAddressGroup initDataDescriptionEntryRename(
			final DataDescriptionEntryRename dataDescriptionEntryRename, final CobolStorageArea memory) {
		final RenamesClause renamesClause = dataDescriptionEntryRename.getRenamesClause();
		final CobolAddressGroup result = addressGroupService.createAddressGroup();

		for (final Call call : renamesClause.getCalls()) {
			final DataDescriptionEntry calledDataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(call);
			final CobolAddressGroup calledAddressGroup = storageService.getAddressGroup(calledDataDescriptionEntry,
					memory);
			result.getAddresses().addAll(calledAddressGroup.getAddresses());
		}

		storageService.putAddressGroup(dataDescriptionEntryRename, result, memory);

		return result;
	}

	private CobolAddressGroup initDataDescriptionEntryScalar(final DataDescriptionEntry dataDescriptionEntry,
			final CobolStorageArea memory) {
		final CobolValue value = valueStmtService.getValueClauseValue(dataDescriptionEntry, memory);
		final Integer length = cobolPictureLengthService.getLength(dataDescriptionEntry);

		final CobolAddressGroup result = addressGroupService.createAddressGroup(value, length);
		storageService.putAddressGroup(dataDescriptionEntry, result, memory);
		return result;
	}
}
