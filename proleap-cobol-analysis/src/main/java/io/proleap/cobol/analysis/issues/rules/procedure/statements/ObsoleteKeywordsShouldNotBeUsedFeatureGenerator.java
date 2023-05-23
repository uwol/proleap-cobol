package io.proleap.cobol.analysis.issues.rules.procedure.statements;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.CobolBaseVisitor;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.SpecialRegisterCall;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.environment.configuration.ConfigurationSection;
import io.proleap.cobol.asg.metamodel.environment.configuration.object.MemorySizeClause;
import io.proleap.cobol.asg.metamodel.environment.configuration.source.SourceComputerParagraph;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.InputOutputSection;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.iocontrol.IoControlParagraph;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.iocontrol.MultipleFileClause;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.iocontrol.RerunClause;
import io.proleap.cobol.asg.metamodel.identification.AuthorParagraph;
import io.proleap.cobol.asg.metamodel.identification.DateCompiledParagraph;
import io.proleap.cobol.asg.metamodel.identification.DateWrittenParagraph;
import io.proleap.cobol.asg.metamodel.identification.IdentificationDivision;
import io.proleap.cobol.asg.metamodel.identification.InstallationParagraph;
import io.proleap.cobol.asg.metamodel.identification.SecurityParagraph;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.GoToStatement;
import io.proleap.cobol.asg.metamodel.procedure.open.Input;
import io.proleap.cobol.asg.metamodel.procedure.open.InputPhrase;
import io.proleap.cobol.asg.metamodel.procedure.open.OpenStatement;
import io.proleap.cobol.asg.metamodel.procedure.stop.StopStatement;
import io.proleap.cobol.asg.metamodel.procedure.use.UseStatement;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class ObsoleteKeywordsShouldNotBeUsedFeatureGenerator extends FeatureGenerator<ASGElement> {

	@Override
	public Stream<ASGElement> getAll(final CompilationUnit compilationUnit) {
		final Stream<ASGElement> result = Stream.concat(obsoleteIdentificationDivision(compilationUnit), Stream.concat(
				obsoleteEnvironmentDivision(compilationUnit),
				Stream.concat(obsoleteDataDivision(compilationUnit), obsoleteProcedureDivision(compilationUnit))));

		return result;
	}

	protected Stream<AuthorParagraph> getAuthorParagraph(final List<IdentificationDivision> identificationDivision) {
		return identificationDivision.stream().filter(entry -> entry != null)
				.map(identificationDiv -> identificationDiv.getAuthorParagraph())
				.filter(authorParagraph -> authorParagraph != null);
	}

	protected Stream<FileDescriptionEntry> getDataRecordsClause(
			final List<FileDescriptionEntry> fileDescriptionEntries) {
		return fileDescriptionEntries.stream().filter(fileDescriptionEntry -> {
			return fileDescriptionEntry != null && fileDescriptionEntry.getDataRecordsClause() != null;
		});
	}

	protected Stream<DateCompiledParagraph> getDateCompiledParagraph(
			final List<IdentificationDivision> identificationDivision) {
		return identificationDivision.stream().map(identificationDiv -> identificationDiv.getDateCompiledParagraph())
				.filter(dateCompiledParagraph -> dateCompiledParagraph != null);
	}

	protected Stream<DateWrittenParagraph> getDateWrittenParagraph(
			final List<IdentificationDivision> identificationDivision) {
		return identificationDivision.stream().map(identificationDiv -> identificationDiv.getDateWrittenParagraph())
				.filter(dateWrittenParagraph -> dateWrittenParagraph != null);
	}

	protected Stream<SourceComputerParagraph> getDebugModeParagraph(
			final List<ConfigurationSection> configurationSection) {
		return configurationSection.stream().map(section -> section.getSourceComputerParagraph())
				.filter(paragraph -> paragraph != null).filter(paragraph -> paragraph.isDebuggingMode());
	}

	protected Stream<InstallationParagraph> getInstallationParagraph(
			final List<IdentificationDivision> identificationDivision) {
		return identificationDivision.stream().map(identificationDiv -> identificationDiv.getInstallationParagraph())
				.filter(installationParagraph -> installationParagraph != null);
	}

	protected Stream<FileDescriptionEntry> getLabelRecordsClause(
			final List<FileDescriptionEntry> fileDescriptionEntries) {
		return fileDescriptionEntries.stream().filter(fileDescriptionEntry -> {
			return fileDescriptionEntry != null && fileDescriptionEntry.getLabelRecordsClause() != null;
		});
	}

	protected Stream<MemorySizeClause> getMemorySizeClause(final List<ConfigurationSection> configurationSection) {
		return configurationSection.stream().map(configSection -> configSection.getObjectComputerParagraph())
				.filter(objectComputerParagraph -> objectComputerParagraph != null)
				.map(objectComputerParagraph -> objectComputerParagraph.getMemorySizeClause())
				.filter(memorySizeClause -> memorySizeClause != null);
	}

	protected Stream<MultipleFileClause> getMultipleFileClause(final List<IoControlParagraph> ioControlParagraph) {
		return ioControlParagraph.stream().map(ioControl -> ioControl.getMultipleFileClause())
				.filter(multipleFileClause -> multipleFileClause != null);
	}

	protected Stream<FileControlEntry> getPaddingCharacterClause(final Stream<InputOutputSection> inputOutputSection) {
		return inputOutputSection.map(ioSection -> ioSection.getFileControlParagraph())
				.filter(fileControl -> fileControl != null)
				.flatMap(fileControl -> fileControl.getFileControlEntries().stream()).filter(fileControlEntry -> {
					return fileControlEntry != null && fileControlEntry.getPaddingCharacterClause() != null;
				});
	}

	protected Stream<RerunClause> getRerunClause(final List<IoControlParagraph> ioControlParagraph) {
		return ioControlParagraph.stream().filter(entry -> entry != null).map(ioControl -> ioControl.getRerunClause())
				.filter(rerunClause -> rerunClause != null);
	}

	protected Stream<SecurityParagraph> getSecurityParagraph(
			final List<IdentificationDivision> identificationDivision) {
		return identificationDivision.stream().filter(entry -> entry != null)
				.map(identificationDiv -> identificationDiv.getSecurityParagraph())
				.filter(securityParagraph -> securityParagraph != null);
	}

	protected Stream<Call> getSpecialRegisterDebugItem(final CompilationUnit compilationUnit) {
		final List<Call> specialRegisterDebugItemCall = new ArrayList<Call>();
		final Stream<ProcedureDivision> procedureDivision = CobolStreamUtils.procedureDivisions(compilationUnit)
				.filter(procedureDiv -> procedureDiv != null);

		final CobolBaseVisitor<Boolean> specialRegisterVisitor = new CobolBaseVisitor<Boolean>() {
			protected boolean addIfRelevantCall(final Call call) {
				if (Call.CallType.SPECIAL_REGISTER_CALL.equals(call.getCallType())) {
					final SpecialRegisterCall specialRegisterCall = (SpecialRegisterCall) call;
					if (SpecialRegisterCall.SpecialRegisterType.DEBUG_ITEM
							.equals(specialRegisterCall.getSpecialRegisterType())) {
						specialRegisterDebugItemCall.add(specialRegisterCall);
						return true;
					}
				}
				return false;
			}

			@Override
			public Boolean visitSpecialRegister(final CobolParser.SpecialRegisterContext ctx) {
				final Call call = (Call) compilationUnit.getProgram().getASGElementRegistry().getASGElement(ctx);
				addIfRelevantCall(call);
				return true;
			}
		};

		procedureDivision.forEach(procDiv -> specialRegisterVisitor.visit(procDiv.getCtx()));

		return specialRegisterDebugItemCall.stream();
	}

	protected Stream<UseStatement> getUseForDebuggingStatement(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.procedureDivisions(compilationUnit).filter(procedureDiv -> procedureDiv != null)
				.map(procedureDiv -> procedureDiv.getDeclaratives()).filter(declarative -> declarative != null)
				.flatMap(declarative -> declarative.getDeclaratives().stream())
				.filter(declarative -> declarative != null).map(declarative -> declarative.getUseStament())
				.filter(useStatement -> {
					return useStatement != null && useStatement.getUseType().equals(UseStatement.UseType.DEBUG);
				});
	}

	protected Stream<FileDescriptionEntry> getValueOfClause(final List<FileDescriptionEntry> fileDescriptionEntries) {
		return fileDescriptionEntries.stream().filter(fileDescriptionEntry -> {
			return fileDescriptionEntry != null && fileDescriptionEntry.getValueOfClause() != null;
		});
	}

	protected boolean isRelevantOpenStatement(final Statement statement) {
		final OpenStatement openStatement = (OpenStatement) statement;
		final List<InputPhrase> inputPhrases = openStatement.getInputPhrases();
		if (inputPhrases == null || inputPhrases.isEmpty()) {
			return false;
		}

		final List<Input> reversedInputs = inputPhrases.stream()
				.flatMap(inputPhrase -> inputPhrase.getInputs().stream()).filter(input -> {
					return input != null && Input.InputType.REVERSED.equals(input.getInputType());
				}).collect(Collectors.toList());

		return reversedInputs != null && !reversedInputs.isEmpty();
	}

	protected boolean isRelevantStatement(final Statement statement) {
		if (statement == null) {
			return false;
		}

		final StatementType statementType = statement.getStatementType();

		if (StatementTypeEnum.OPEN.equals(statementType)) {
			return isRelevantOpenStatement(statement);
		}

		if (StatementTypeEnum.ALTER.equals(statementType)) {
			return true;
		}

		if (StatementTypeEnum.STOP.equals(statementType)) {
			return ((StopStatement) statement).getDisplayValueStmt() != null;
		}

		if (StatementTypeEnum.GO_TO.equals(statementType)) {
			return GoToStatement.GoToType.SIMPLE.equals(((GoToStatement) statement).getGoToType());
		}

		return false;
	}

	protected Stream<ASGElement> obsoleteConfigurationSection(final List<ConfigurationSection> configurationSection) {
		final List<ASGElement> result = new ArrayList<ASGElement>();

		result.addAll(getDebugModeParagraph(configurationSection).collect(Collectors.toList()));
		result.addAll(getMemorySizeClause(configurationSection).collect(Collectors.toList()));

		return result.stream();
	}

	protected Stream<ASGElement> obsoleteDataDivision(final CompilationUnit compilationUnit) {
		final List<ASGElement> result = new ArrayList<ASGElement>();

		result.addAll(obsoleteFileSection(compilationUnit).collect(Collectors.toList()));

		return result.stream();
	}

	protected Stream<ASGElement> obsoleteEnvironmentDivision(final CompilationUnit compilationUnit) {
		final List<ASGElement> result = new ArrayList<ASGElement>();
		final List<ConfigurationSection> configurationSection = CobolStreamUtils.environmentDivisions(compilationUnit)
				.filter(div -> div != null).map(div -> div.getConfigurationSection())
				.filter(configSection -> configSection != null).collect(Collectors.toList());

		result.addAll(obsoleteInputOutputSection(compilationUnit).collect(Collectors.toList()));
		result.addAll(obsoleteConfigurationSection(configurationSection).collect(Collectors.toList()));

		return result.stream();

	}

	protected Stream<ASGElement> obsoleteFileControl(final CompilationUnit compilationUnit) {
		final List<ASGElement> result = new ArrayList<ASGElement>();
		final Stream<InputOutputSection> inputOutputSection = CobolStreamUtils.environmentDivisions(compilationUnit)
				.filter(envDiv -> envDiv != null).map(envDiv -> envDiv.getInputOutputSection())
				.filter(ioSection -> ioSection != null);

		result.addAll(getPaddingCharacterClause(inputOutputSection).collect(Collectors.toList()));

		return result.stream();
	}

	protected Stream<ASGElement> obsoleteFileSection(final CompilationUnit compilationUnit) {
		final List<ASGElement> result = new ArrayList<ASGElement>();
		final List<FileDescriptionEntry> fileDescriptionEntries = CobolStreamUtils.fileSections(compilationUnit)
				.filter(fs -> fs != null).flatMap(fs -> {
					return fs.getFileDescriptionEntries().stream();
				}).filter(fileDescriptionEntry -> fileDescriptionEntry != null).collect(Collectors.toList());

		result.addAll(getDataRecordsClause(fileDescriptionEntries).collect(Collectors.toList()));
		result.addAll(getValueOfClause(fileDescriptionEntries).collect(Collectors.toList()));
		result.addAll(getLabelRecordsClause(fileDescriptionEntries).collect(Collectors.toList()));

		return result.stream();
	}

	protected Stream<ASGElement> obsoleteIdentificationDivision(final CompilationUnit compilationUnit) {
		final List<ASGElement> result = new ArrayList<ASGElement>();
		final List<IdentificationDivision> identificationDivision = CobolStreamUtils
				.identificationDivisions(compilationUnit).filter(division -> division != null)
				.collect(Collectors.toList());

		result.addAll(getAuthorParagraph(identificationDivision).collect(Collectors.toList()));
		result.addAll(getInstallationParagraph(identificationDivision).collect(Collectors.toList()));
		result.addAll(getDateWrittenParagraph(identificationDivision).collect(Collectors.toList()));
		result.addAll(getDateCompiledParagraph(identificationDivision).collect(Collectors.toList()));
		result.addAll(getSecurityParagraph(identificationDivision).collect(Collectors.toList()));

		return result.stream();
	}

	protected Stream<ASGElement> obsoleteInputOutputSection(final CompilationUnit compilationUnit) {
		final List<ASGElement> result = new ArrayList<ASGElement>();
		final List<IoControlParagraph> ioControlParagraph = CobolStreamUtils.environmentDivisions(compilationUnit)
				.filter(environmentDivision -> environmentDivision != null)
				.map(environmentDivision -> environmentDivision.getInputOutputSection())
				.filter(section -> section != null)
				.map(inputOutputSection -> inputOutputSection.getIoControlParagraph())
				.filter(ioControl -> ioControl != null).collect(Collectors.toList());

		result.addAll(getRerunClause(ioControlParagraph).collect(Collectors.toList()));
		result.addAll(getMultipleFileClause(ioControlParagraph).collect(Collectors.toList()));
		result.addAll(obsoleteFileControl(compilationUnit).collect(Collectors.toList()));

		return result.stream();
	}

	protected Stream<ASGElement> obsoleteProcedureDivision(final CompilationUnit compilationUnit) {
		final List<ASGElement> result = new ArrayList<ASGElement>();

		result.addAll(obsoleteProcedureStatements(compilationUnit).collect(Collectors.toList()));
		result.addAll(getSpecialRegisterDebugItem(compilationUnit).collect(Collectors.toList()));
		result.addAll(getUseForDebuggingStatement(compilationUnit).collect(Collectors.toList()));

		return result.stream();
	}

	protected Stream<Statement> obsoleteProcedureStatements(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return statement != null && isRelevantStatement(statement);
		});
	}
}
