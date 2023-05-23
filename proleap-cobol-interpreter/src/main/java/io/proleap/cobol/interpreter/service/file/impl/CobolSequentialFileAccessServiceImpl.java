package io.proleap.cobol.interpreter.service.file.impl;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.commons.value.CobolValueComparatorService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.impl.CobolStringValueImpl;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;
import io.proleap.cobol.interpreter.domain.file.CobolSequentialFile;
import io.proleap.cobol.interpreter.service.address.CobolAddressGroupService;
import io.proleap.cobol.interpreter.service.address.CobolAddressService;
import io.proleap.cobol.interpreter.service.file.CobolSequentialFileAccessService;
import io.proleap.cobol.interpreter.service.file.domain.KeyCriterion;
import io.proleap.cobol.interpreter.service.file.domain.SortCriterion;

@Singleton
public class CobolSequentialFileAccessServiceImpl implements CobolSequentialFileAccessService {

	public class CobolFileRecordComparator implements Comparator<CobolAddressGroup> {

		private final ProgramUnit programUnit;

		private final List<SortCriterion> sortCriteria;

		public CobolFileRecordComparator(final List<SortCriterion> sortCriteria, final ProgramUnit programUnit) {
			this.sortCriteria = sortCriteria;
			this.programUnit = programUnit;
		}

		@Override
		public int compare(final CobolAddressGroup addressGroup1, final CobolAddressGroup addressGroup2) {
			final List<CobolAddress> addresses1 = addressGroup1.getAddresses();
			final List<CobolAddress> addresses2 = addressGroup2.getAddresses();

			for (final SortCriterion sortCriterion : sortCriteria) {
				for (final Integer position : sortCriterion.positions) {
					final CobolAddress address1 = addresses1.get(position);
					final CobolAddress address2 = addresses2.get(position);
					final Integer comparison = cobolValueComparatorService.compare(address1.getValue(),
							address2.getValue(), programUnit);

					if (comparison == null) {
					} else if (comparison != 0 && sortCriterion.descending) {
						return comparison * -1;
					} else {
						return comparison;
					}
				}
			}

			return 0;
		}
	}

	@Inject
	private CobolAddressGroupService addressGroupService;

	@Inject
	private CobolAddressService addressService;

	@Inject
	private CobolValueComparatorService cobolValueComparatorService;

	@Inject
	private CobolValueComparatorService valueComparatorService;

	@Inject
	private CobolValueService valueService;

	@Override
	public void close(final CobolSequentialFile file) {
		file.setOpen(false);
	}

	@Override
	public void copy(final CobolSequentialFile from, final CobolSequentialFile to) {
		to.getRecords().addAll(from.getRecords());
	}

	@Override
	public void delete(final CobolSequentialFile file, final List<CobolAddress> addresses,
			final KeyCriterion keyCriterion, final ProgramUnit programUnit) {
		do {
			final String record = file.getRecords().get(file.getIndex());
			final boolean matchesKey = matchesKeyCriterion(record, addresses, keyCriterion, programUnit);

			if (Boolean.TRUE.equals(matchesKey)) {
				file.getRecords().remove(file.getIndex());
				break;
			} else {
				file.next();
			}
		} while (file.hasNext());
	}

	private boolean matchesKeyCriterion(final String record, final List<CobolAddress> fileControlEntryAddresses,
			final KeyCriterion keyCriterion, final ProgramUnit programUnit) {
		boolean result = true;

		if (keyCriterion == null) {
		} else {
			final List<CobolValue> recordValues = addressService.splitValue(CobolStringValueImpl.of(record),
					fileControlEntryAddresses, programUnit);
			final CobolAddressGroup recordAddressGroup = addressGroupService.createAddressGroup(recordValues,
					fileControlEntryAddresses);
			final List<CobolAddress> recordAddresses = recordAddressGroup.getAddresses();

			for (final Integer position : keyCriterion.positions) {
				final CobolAddress keyRecordAddressGroup = recordAddresses.get(position);
				final CobolValue keyRecordValue = keyRecordAddressGroup.getValue();
				final Boolean addressMatches = valueComparatorService.equal(keyCriterion.keyValue, keyRecordValue,
						programUnit);

				if (!Boolean.TRUE.equals(addressMatches)) {
					result = false;
					break;
				}
			}
		}

		return result;
	}

	@Override
	public void open(final CobolSequentialFile file) {
		file.setOpen(true);
	}

	@Override
	public String read(final CobolSequentialFile file, final List<CobolAddress> addresses,
			final KeyCriterion keyCriterion, final ProgramUnit programUnit) {
		while (file.hasNext()) {
			final String record = file.next();
			final boolean matchesKey = matchesKeyCriterion(record, addresses, keyCriterion, programUnit);

			if (Boolean.TRUE.equals(matchesKey)) {
				return record;
			}
		}

		return null;
	}

	@Override
	public void rewrite(final String record, final CobolSequentialFile file) {
		file.getRecords().set(file.getIndex(), record);
	}

	@Override
	public void sort(final CobolSequentialFile file, final List<CobolAddress> fileControlEntryAddresses,
			final List<SortCriterion> sortCriteria, final ProgramUnit programUnit) {
		final CobolFileRecordComparator recordComparator = new CobolFileRecordComparator(sortCriteria, programUnit);
		final List<CobolAddressGroup> addressGroups = new ArrayList<>();

		for (final String record : file.getRecords()) {
			final List<CobolValue> values = addressService.splitValue(CobolStringValueImpl.of(record),
					fileControlEntryAddresses, programUnit);
			addressGroups.add(addressGroupService.createAddressGroup(values, fileControlEntryAddresses));
		}

		addressGroups.sort(recordComparator);

		final List<String> result = new ArrayList<>();

		for (final CobolAddressGroup addressGroup : addressGroups) {
			final List<CobolValue> values = addressGroup.getAddresses().stream().map(address -> {
				return address.getValue();
			}).collect(Collectors.toList());

			final CobolValue mergedValue = addressService.mergeValues(values, programUnit);
			final String mergedString = valueService.getAsString(mergedValue, programUnit);
			result.add(mergedString);
		}

		file.getRecords().clear();
		file.getRecords().addAll(result);
	}

	@Override
	public void start(final CobolSequentialFile file) {
		file.setIndex(CobolSequentialFile.INITIAL_INDEX);
	}

	@Override
	public void write(final String record, final CobolSequentialFile file) {
		file.getRecords().add(record);
	}
}
