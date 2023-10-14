package io.proleap.cobol.interpreter.service.file.criteria.impl;

import java.util.ArrayList;
import java.util.List;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.AlternateRecordKeyClause;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.RecordKeyClause;
import io.proleap.cobol.asg.metamodel.procedure.read.Key;
import io.proleap.cobol.asg.metamodel.procedure.sort.OnKey;
import io.proleap.cobol.asg.metamodel.procedure.sort.OnKey.OnKeyType;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.file.criteria.CobolFileCriteriaService;
import io.proleap.cobol.interpreter.service.file.domain.KeyCriterion;
import io.proleap.cobol.interpreter.service.file.domain.SortCriterion;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class CobolFileCriteriaServiceImpl implements CobolFileCriteriaService {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolStorageService storageService;

	@Override
	public KeyCriterion createKeyCriterion(final AlternateRecordKeyClause alternateRecordKeyClause,
			final List<CobolAddress> addresses, final CobolInterpreterParams params) {
		final KeyCriterion result;

		if (alternateRecordKeyClause == null) {
			result = null;
		} else {
			final Call keyCall = alternateRecordKeyClause.getDataCall();
			result = createKeyCriterion(keyCall, addresses, params);
		}

		return result;
	}

	private KeyCriterion createKeyCriterion(final Call keyCall, final List<CobolAddress> addresses,
			final CobolInterpreterParams params) {
		final DataDescriptionEntry keyDataDescriptionEntry = dataDescriptionEntryService
				.getDataDescriptionEntry(keyCall);
		final CobolAddressGroup keyAddressGroup = storageService.getAddressGroup(keyDataDescriptionEntry,
				params.getState().getStorage());

		final KeyCriterion result = new KeyCriterion();
		result.keyValue = storageService.getValue(keyDataDescriptionEntry, params.getState().getStorage());

		for (final CobolAddress keyAddress : keyAddressGroup.getAddresses()) {
			final int position = addresses.indexOf(keyAddress);
			result.positions.add(position);
		}

		return result;
	}

	@Override
	public KeyCriterion createKeyCriterion(final Key key, final List<CobolAddress> addresses,
			final CobolInterpreterParams params) {
		final KeyCriterion result;

		if (key == null) {
			result = null;
		} else {
			final Call keyCall = key.getKeyCall();
			result = createKeyCriterion(keyCall, addresses, params);
		}

		return result;
	}

	@Override
	public KeyCriterion createKeyCriterion(final RecordKeyClause recordKeyClause, final List<CobolAddress> addresses,
			final CobolInterpreterParams params) {
		final KeyCriterion result;

		if (recordKeyClause == null) {
			result = null;
		} else {
			final Call keyCall = recordKeyClause.getRecordKeyCall();
			result = createKeyCriterion(keyCall, addresses, params);
		}

		return result;
	}

	@Override
	public List<SortCriterion> createSortCriteria(final List<CobolAddress> addresses, final List<OnKey> onKeys,
			final CobolInterpreterParams params) {
		final List<SortCriterion> result = new ArrayList<>();

		for (final OnKey onKey : onKeys) {
			for (final Call call : onKey.getKeyCalls()) {
				final DataDescriptionEntry onEntry = dataDescriptionEntryService.getDataDescriptionEntry(call);
				final CobolAddressGroup addressGroup = storageService.getAddressGroup(onEntry,
						params.getState().getStorage());

				final SortCriterion sortCriterion = new SortCriterion();
				sortCriterion.descending = OnKeyType.DESCENDING.equals(onKey.getOnKeyType());

				for (final CobolAddress address : addressGroup.getAddresses()) {
					final int position = addresses.indexOf(address);
					sortCriterion.positions.add(position);
				}

				result.add(sortCriterion);
			}
		}

		return result;
	}
}
