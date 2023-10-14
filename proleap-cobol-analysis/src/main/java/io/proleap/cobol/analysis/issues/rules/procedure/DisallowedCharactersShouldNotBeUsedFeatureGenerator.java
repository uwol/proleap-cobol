package io.proleap.cobol.analysis.issues.rules.procedure;

import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class DisallowedCharactersShouldNotBeUsedFeatureGenerator extends FeatureGenerator<ASGElement> {

	private static final String ALLOWED_CHARECTERS_REGEXP = "[a-zA-Z0-9-]+";

	@Override
	public Stream<ASGElement> getAll(final CompilationUnit compilationUnit) {
		return Stream.concat(Stream.concat(getDataDescriptionEntries(compilationUnit), getParagraphs(compilationUnit)),
				getSections(compilationUnit));
	}

	protected Stream<ASGElement> getDataDescriptionEntries(final CompilationUnit compilationUnit) {
		return Stream.concat(getWorkingStorageDataDescriptionEntries(compilationUnit),
				getLinkageSectionDataDescriptionEntries(compilationUnit));
	}

	protected Stream<DataDescriptionEntry> getLinkageSectionDataDescriptionEntries(
			final CompilationUnit compilationUnit) {
		return CobolStreamUtils.linkageSectionDataDescriptionEntries(compilationUnit).filter(dataDescriptionEntry -> {
			return isRelevantDataDescriptionEntry(dataDescriptionEntry);
		});
	}

	protected Stream<Paragraph> getParagraphs(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.paragraphs(compilationUnit).filter(paragraph -> {
			return isRelevantParagraph(paragraph);
		});
	}

	protected Stream<Section> getSections(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.sections(compilationUnit).filter(section -> {
			return isRelevantSection(section);
		});
	}

	protected Stream<DataDescriptionEntry> getWorkingStorageDataDescriptionEntries(
			final CompilationUnit compilationUnit) {
		return CobolStreamUtils.workingStorageSectionDataDescriptionEntries(compilationUnit)
				.filter(dataDescriptionEntry -> {
					return isRelevantDataDescriptionEntry(dataDescriptionEntry);
				});
	}

	protected boolean isRelevantDataDescriptionEntry(final DataDescriptionEntry dataDescriptionEntry) {
		if (DataDescriptionEntry.DataDescriptionEntryType.GROUP
				.equals(dataDescriptionEntry.getDataDescriptionEntryType())) {
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
			if (dataDescriptionEntryGroup.getFiller() != null && dataDescriptionEntryGroup.getFiller()) {
				return false;
			}
		}
		return !nameContainsOnlyAllowedCharecters(dataDescriptionEntry);
	}

	protected boolean isRelevantParagraph(final Paragraph paragraph) {
		return !nameContainsOnlyAllowedCharecters(paragraph);
	}

	protected boolean isRelevantSection(final Section section) {
		return !nameContainsOnlyAllowedCharecters(section);

	}

	protected boolean nameContainsOnlyAllowedCharecters(final DataDescriptionEntry dataDescriptionEntry) {
		if (dataDescriptionEntry == null) {
			return false;
		} else if (dataDescriptionEntry.getName() == null) {
			return false;
		} else {
			final String dataDescriptionName = dataDescriptionEntry.getName();
			if (!dataDescriptionName.matches(ALLOWED_CHARECTERS_REGEXP)) {
				return false;
			}
		}
		return true;
	}

	protected boolean nameContainsOnlyAllowedCharecters(final Paragraph paragraph) {
		if (paragraph == null) {
			return false;
		} else if (paragraph.getName() == null) {
			return false;
		} else if (!paragraph.getName().matches(ALLOWED_CHARECTERS_REGEXP)) {
			return false;
		}
		return true;
	}

	protected boolean nameContainsOnlyAllowedCharecters(final Section section) {
		if (section == null) {
			return false;
		} else if (section.getName() == null) {
			return false;
		} else if (!section.getName().matches(ALLOWED_CHARECTERS_REGEXP)) {
			return false;
		}
		return true;
	}
}
