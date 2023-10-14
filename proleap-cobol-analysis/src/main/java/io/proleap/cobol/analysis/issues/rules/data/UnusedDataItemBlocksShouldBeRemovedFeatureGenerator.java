package io.proleap.cobol.analysis.issues.rules.data;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolBaseVisitor;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.DataDescriptionEntryCall;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class UnusedDataItemBlocksShouldBeRemovedFeatureGenerator extends FeatureGenerator<DataDescriptionEntry> {

	protected boolean checkForTopLevelDataItem(final DataDescriptionEntry dataDescriptionEntry) {
		if (DataDescriptionEntryType.GROUP.equals(dataDescriptionEntry.getDataDescriptionEntryType())) {
			if (dataDescriptionEntry.getParentDataDescriptionEntryGroup() == null) {
				final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;

				if (dataDescriptionEntryGroup.getFiller() == null
						&& dataDescriptionEntryGroup.getDataDescriptionEntries() != null
						&& !dataDescriptionEntryGroup.getDataDescriptionEntries().isEmpty()) {
					return true;
				}
			}
		}
		return false;
	}

	protected List<DataDescriptionEntry> filterUnusedDataItemsFromCalledDataItems(
			final List<DataDescriptionEntry> dataDescriptionEntries,
			final List<DataDescriptionEntry> calledDataDescriptionEntries) {

		final List<DataDescriptionEntry> result = new ArrayList<DataDescriptionEntry>();

		for (final DataDescriptionEntry dataDescriptionEntry : dataDescriptionEntries) {
			if (isRelevantDataDescriptionEntry(dataDescriptionEntry, calledDataDescriptionEntries)) {
				result.add(dataDescriptionEntry);
			} else {
				final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
				final List<DataDescriptionEntry> subDataDescriptionEntries = dataDescriptionEntryGroup
						.getDataDescriptionEntries();

				for (final DataDescriptionEntry subDataDescriptionEntry : subDataDescriptionEntries) {
					result.remove(subDataDescriptionEntry);
				}
			}
		}
		return result;
	}

	@Override
	public Stream<DataDescriptionEntry> getAll(final CompilationUnit compilationUnit) {
		final List<DataDescriptionEntry> possibleDataItemBlocks = getDataDescriptionEntries(compilationUnit)
				.filter(dataDescriptionEntry -> {
					return checkForTopLevelDataItem(dataDescriptionEntry);
				}).collect(Collectors.toList());

		final List<DataDescriptionEntry> calledEntries = getCalledDataDescriptonEntries(compilationUnit)
				.collect(Collectors.toList());

		return filterUnusedDataItemsFromCalledDataItems(possibleDataItemBlocks, calledEntries).stream();
	}

	protected Stream<DataDescriptionEntry> getCalledDataDescriptonEntries(final CompilationUnit compilationUnit) {
		final List<DataDescriptionEntry> calledDataDescriptionEntries = new ArrayList<DataDescriptionEntry>();

		final CobolBaseVisitor<Boolean> identifierVisitor = new CobolBaseVisitor<Boolean>() {

			protected boolean isRelevantCall(final Call call) {
				if (Call.CallType.DATA_DESCRIPTION_ENTRY_CALL.equals(call.getCallType())) {
					return true;
				}

				return false;
			}

			@Override
			public Boolean visitIdentifier(final CobolParser.IdentifierContext ctx) {
				final Call call = (Call) compilationUnit.getProgram().getASGElementRegistry().getASGElement(ctx);

				if (call == null) {
					return false;
				} else if (isRelevantCall(call)) {
					final DataDescriptionEntryCall dataDescriptionEntryCall = (DataDescriptionEntryCall) call.unwrap();
					final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryCall
							.getDataDescriptionEntry();

					if (calledDataDescriptionEntries.contains(dataDescriptionEntry)) {
						return false;
					} else if (calledDataDescriptionEntries
							.contains(dataDescriptionEntry.getParentDataDescriptionEntryGroup())) {
						return false;
					}

					calledDataDescriptionEntries.add(dataDescriptionEntry);
				}

				return true;
			}
		};

		identifierVisitor.visit(compilationUnit.getCtx());
		return calledDataDescriptionEntries.stream();
	}

	protected Stream<DataDescriptionEntry> getDataDescriptionEntries(final CompilationUnit compilationUnit) {
		return Stream.concat(CobolStreamUtils.linkageSectionDataDescriptionEntries(compilationUnit),
				CobolStreamUtils.workingStorageSectionDataDescriptionEntries(compilationUnit));
	}

	protected boolean isRelevantDataDescriptionEntry(final DataDescriptionEntry dataDescriptionEntry,
			final List<DataDescriptionEntry> calledDataDescriptionEntries) {
		if (calledDataDescriptionEntries.contains(dataDescriptionEntry)) {
			return false;
		}

		final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;

		if (dataDescriptionEntryGroup != null && dataDescriptionEntryGroup.getDataDescriptionEntries() != null
				&& !dataDescriptionEntryGroup.getDataDescriptionEntries().isEmpty()
				&& dataDescriptionEntryGroup.getFiller() != null) {
			return false;
		}
		if (dataDescriptionEntryGroup.getValueClause() != null) {
			return false;
		}

		final List<DataDescriptionEntry> dataDescriptionEntries = dataDescriptionEntryGroup.getDataDescriptionEntries();

		for (final DataDescriptionEntry subDataDescriptionEntry : dataDescriptionEntries) {
			if (!isRelevantDataDescriptionEntry(subDataDescriptionEntry, calledDataDescriptionEntries)) {
				return false;
			}
		}
		return true;
	}
}
