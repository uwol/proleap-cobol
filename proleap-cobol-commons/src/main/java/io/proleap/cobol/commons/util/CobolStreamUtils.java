package io.proleap.cobol.commons.util;

import java.util.stream.Stream;

import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.DataDivision;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.file.FileSection;
import io.proleap.cobol.asg.metamodel.data.linkage.LinkageSection;
import io.proleap.cobol.asg.metamodel.data.localstorage.LocalStorageSection;
import io.proleap.cobol.asg.metamodel.data.workingstorage.WorkingStorageSection;
import io.proleap.cobol.asg.metamodel.environment.EnvironmentDivision;
import io.proleap.cobol.asg.metamodel.environment.configuration.ConfigurationSection;
import io.proleap.cobol.asg.metamodel.environment.configuration.source.SourceComputerParagraph;
import io.proleap.cobol.asg.metamodel.identification.IdentificationDivision;
import io.proleap.cobol.asg.metamodel.identification.ProgramIdParagraph;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.commons.stream.CobolNestedStreams;

public class CobolStreamUtils {

	public static Stream<ConfigurationSection> configurationSections(final CompilationUnit compilationUnit) {
		return environmentDivisions(compilationUnit).filter(environmentDivision -> {
			return environmentDivision.getConfigurationSection() != null;
		}).map(environmentDivision -> environmentDivision.getConfigurationSection());
	}

	public static Stream<DataDescriptionEntry> dataDescriptionEntriesRec(final CompilationUnit compilationUnit) {
		return Stream.concat(
				Stream.concat(localStorageDataDescriptionEntries(compilationUnit),
						fileSectionDataDescriptionEntries(compilationUnit)),
				Stream.concat(workingStorageSectionDataDescriptionEntries(compilationUnit),
						linkageSectionDataDescriptionEntries(compilationUnit)));
	}

	public static Stream<DataDivision> dataDivisions(final CompilationUnit compilationUnit) {
		return programUnits(compilationUnit).filter(programUnit -> {
			return programUnit.getDataDivision() != null;
		}).map(programUnit -> {
			return programUnit.getDataDivision();
		});
	}

	public static Stream<EnvironmentDivision> environmentDivisions(final CompilationUnit compilationUnit) {
		return programUnits(compilationUnit).filter(programUnit -> {
			return programUnit.getEnvironmentDivision() != null;
		}).map(programUnit -> {
			return programUnit.getEnvironmentDivision();
		});
	}

	public static Stream<DataDescriptionEntry> fileSectionDataDescriptionEntries(
			final CompilationUnit compilationUnit) {
		return fileSectionFileDescriptionEntries(compilationUnit)
				.flatMap(fileDescriptionEntry -> fileDescriptionEntry.getDataDescriptionEntries().stream());
	}

	public static Stream<FileDescriptionEntry> fileSectionFileDescriptionEntries(
			final CompilationUnit compilationUnit) {
		return fileSections(compilationUnit).filter(fileSection -> fileSection.getFileDescriptionEntries() != null)
				.flatMap(fileSection -> fileSection.getFileDescriptionEntries().stream());
	}

	public static Stream<FileSection> fileSections(final CompilationUnit compilationUnit) {
		return dataDivisions(compilationUnit).filter(dataDivision -> dataDivision.getFileSection() != null)
				.map(dataDivision -> dataDivision.getFileSection());
	}

	public static Stream<IdentificationDivision> identificationDivisions(final CompilationUnit compilationUnit) {
		return programUnits(compilationUnit).filter(programUnit -> {
			return programUnit.getIdentificationDivision() != null;
		}).map(programUnit -> {
			return programUnit.getIdentificationDivision();
		});
	}

	public static Stream<DataDescriptionEntry> linkageSectionDataDescriptionEntries(
			final CompilationUnit compilationUnit) {
		return linkageSections(compilationUnit)
				.flatMap(linkageSection -> linkageSection.getDataDescriptionEntries().stream());
	}

	public static Stream<LinkageSection> linkageSections(final CompilationUnit compilationUnit) {
		return dataDivisions(compilationUnit).filter(dataDivision -> {
			return dataDivision.getLinkageSection() != null;
		}).map(dataDivision -> dataDivision.getLinkageSection());
	}

	public static Stream<DataDescriptionEntry> localStorageDataDescriptionEntries(
			final CompilationUnit compilationUnit) {
		return localStorageSections(compilationUnit)
				.filter(localStorageSection -> localStorageSection.getDataDescriptionEntries() != null)
				.flatMap(localStorageSection -> localStorageSection.getDataDescriptionEntries().stream());
	}

	public static Stream<LocalStorageSection> localStorageSections(final CompilationUnit compilationUnit) {
		return dataDivisions(compilationUnit).filter(dataDivision -> {
			return dataDivision.getLocalStorageSection() != null;
		}).map(dataDivision -> dataDivision.getLocalStorageSection());
	}

	public static Stream<Paragraph> paragraphs(final CompilationUnit compilationUnit) {
		return procedureDivisions(compilationUnit).flatMap(procedureDivision -> {
			return procedureDivision.getParagraphs().stream();
		});
	}

	public static Stream<Statement> paragraphStatements(final CompilationUnit compilationUnit) {
		return paragraphs(compilationUnit).flatMap(paragraph -> {
			return paragraph.getStatements().stream();
		});
	}

	public static Stream<ProcedureDivision> procedureDivisions(final CompilationUnit compilationUnit) {
		return programUnits(compilationUnit).filter(programUnit -> {
			return programUnit.getProcedureDivision() != null;
		}).map(programUnit -> {
			return programUnit.getProcedureDivision();
		});
	}

	public static Stream<ProgramIdParagraph> programIdParagraph(final CompilationUnit compilationUnit) {
		return identificationDivisions(compilationUnit)
				.map(identificationDivision -> identificationDivision.getProgramIdParagraph());
	}

	public static Stream<ProgramUnit> programUnits(final CompilationUnit compilationUnit) {
		return compilationUnit.getProgramUnits().stream();
	}

	public static Stream<Section> sections(final CompilationUnit compilationUnit) {
		return procedureDivisions(compilationUnit).flatMap(procedureDivision -> {
			return procedureDivision.getSections().stream();
		});
	}

	public static Stream<Statement> sectionStatements(final CompilationUnit compilationUnit) {
		return sections(compilationUnit).flatMap(section -> {
			return section.getStatements().stream();
		});
	}

	public static Stream<SourceComputerParagraph> sourceComputerParagraphs(final CompilationUnit compilationUnit) {
		return configurationSections(compilationUnit).filter(configurationSection -> {
			return configurationSection.getSourceComputerParagraph() != null;
		}).map(configurationSection -> configurationSection.getSourceComputerParagraph());
	}

	public static Stream<Statement> statements(final CompilationUnit compilationUnit) {
		return procedureDivisions(compilationUnit).flatMap(procedureDivision -> {
			return procedureDivision.getStatements().stream();
		});
	}

	public static Stream<Statement> statementsFlat(final CompilationUnit compilationUnit) {
		return Stream.concat(Stream.concat(statements(compilationUnit), sectionStatements(compilationUnit)),
				paragraphStatements(compilationUnit));
	}

	public static Stream<Statement> statementsRec(final CompilationUnit compilationUnit) {
		return Stream.concat(statementsFlat(compilationUnit),
				statementsFlat(compilationUnit).filter(statement -> statement != null).flatMap(statement -> {
					return CobolNestedStreams.nestedStatementsRec(statement);
				}));
	}

	public static Stream<DataDescriptionEntry> workingStorageSectionDataDescriptionEntries(
			final CompilationUnit compilationUnit) {
		return workingStorageSections(compilationUnit)
				.flatMap(workingStorageSection -> workingStorageSection.getDataDescriptionEntries().stream());
	}

	public static Stream<WorkingStorageSection> workingStorageSections(final CompilationUnit compilationUnit) {
		return dataDivisions(compilationUnit).filter(dataDivision -> {
			return dataDivision.getWorkingStorageSection() != null;
		}).map(dataDivision -> dataDivision.getWorkingStorageSection());
	}
}
