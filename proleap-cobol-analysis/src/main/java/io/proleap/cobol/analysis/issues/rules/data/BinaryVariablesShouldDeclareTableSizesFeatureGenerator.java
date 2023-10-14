package io.proleap.cobol.analysis.issues.rules.data;

import java.util.List;
import java.util.stream.Stream;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.OccursClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.OccursDepending;
import io.proleap.cobol.asg.metamodel.data.datadescription.UsageClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.UsageClause.UsageClauseType;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class BinaryVariablesShouldDeclareTableSizesFeatureGenerator extends FeatureGenerator<DataDescriptionEntry> {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Override
	public Stream<DataDescriptionEntry> getAll(final CompilationUnit compilationUnit) {
		return Stream.concat(getWSDataDescriptionEntries(compilationUnit),
				getLSDataDescriptionEntries(compilationUnit));
	}

	protected Stream<DataDescriptionEntry> getLSDataDescriptionEntries(final CompilationUnit compilationUnit) {
		final Stream<DataDescriptionEntry> dataDescriptionEntries = CobolStreamUtils
				.workingStorageSectionDataDescriptionEntries(compilationUnit);
		return dataDescriptionEntries.filter(dataDescriptionEntry -> {
			return isRelevantDataDescriptionEntry(dataDescriptionEntry);
		});
	}

	protected Stream<DataDescriptionEntry> getWSDataDescriptionEntries(final CompilationUnit compilationUnit) {
		final Stream<DataDescriptionEntry> dataDescriptionEntries = CobolStreamUtils
				.linkageSectionDataDescriptionEntries(compilationUnit);
		return dataDescriptionEntries.filter(dataDescriptionEntry -> {
			return isRelevantDataDescriptionEntry(dataDescriptionEntry);
		});
	}

	protected boolean isBinaryVariable(final Call dependingOnCall) {
		final DataDescriptionEntry dataDescriptionEntry = cobolDataDescriptionEntryService
				.getDataDescriptionEntry(dependingOnCall);
		final boolean result;

		if (dataDescriptionEntry == null) {
			result = false;
		} else {
			final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry
					.getDataDescriptionEntryType();

			if (DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)
					|| DataDescriptionEntryType.SCALAR.equals(dataDescriptionEntryType)) {
				final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
				final UsageClause usageClause = dataDescriptionEntryGroup.getUsageClause();

				if (usageClause == null) {
					result = false;
				} else {
					final UsageClauseType usageClauseType = usageClause.getUsageClauseType();

					switch (usageClauseType) {
					case BINARY:
						result = true;
						break;
					case BINARY_EXTENDED:
						result = true;
						break;
					case BINARY_TRUNCATED:
						result = true;
						break;
					case COMP:
						result = true;
						break;
					case COMP_4:
						result = true;
						break;
					case COMP_5:
						result = true;
						break;
					default:
						result = false;
						break;
					}
				}
			} else {
				result = false;
			}
		}

		return result;
	}

	protected boolean isRelevantDataDescriptionEntry(final DataDescriptionEntry dataDescriptionEntry) {
		final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry.getDataDescriptionEntryType();
		final boolean result;

		if (DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)
				|| DataDescriptionEntryType.SCALAR.equals(dataDescriptionEntryType)) {
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
			final List<OccursClause> occursClauses = dataDescriptionEntryGroup.getOccursClauses();

			if (occursClauses == null) {
				result = false;
			} else {
				occursClauses.removeIf(occur -> {
					final OccursDepending occursDepending = occur.getOccursDepending();

					if (occursDepending != null) {
						final Call call = occursDepending.getDependingOnCall();

						if (call != null) {
							return isBinaryVariable(call);
						} else {
							return true;
						}
					} else {
						return true;
					}
				});

				if (occursClauses.isEmpty()) {
					result = false;
				} else {
					result = true;
				}
			}
		} else {
			result = false;
		}

		return result;
	}
}
