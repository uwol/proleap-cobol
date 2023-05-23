package io.proleap.cobol.analysis.issues.rules.procedure.open;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.FileControlEntryCall;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.close.CloseStatement;
import io.proleap.cobol.asg.metamodel.procedure.open.ExtendPhrase;
import io.proleap.cobol.asg.metamodel.procedure.open.InputOutputPhrase;
import io.proleap.cobol.asg.metamodel.procedure.open.InputPhrase;
import io.proleap.cobol.asg.metamodel.procedure.open.OpenStatement;
import io.proleap.cobol.asg.metamodel.procedure.open.OutputPhrase;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class OpenFilesShouldBeClosedExplicitlyFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		final List<FileDescriptionEntry> closedFiles = CobolStreamUtils.statementsRec(compilationUnit)
				.filter(statement -> statement != null && StatementTypeEnum.CLOSE.equals(statement.getStatementType()))
				.map(statement -> (CloseStatement) statement).flatMap(closeStatement -> getClosedFiles(closeStatement))
				.collect(Collectors.toList());

		final List<Statement> relevantOpenStatements = CobolStreamUtils.statementsRec(compilationUnit)
				.filter(statement -> statement != null && StatementTypeEnum.OPEN.equals(statement.getStatementType()))
				.map(statement -> (OpenStatement) statement)
				.filter(statement -> isRelevantStatement(statement, closedFiles)).collect(Collectors.toList());

		return relevantOpenStatements.stream();
	}

	protected Stream<FileDescriptionEntry> getClosedFiles(final CloseStatement closeStatement) {
		return closeStatement.getCloseFiles().stream().filter(closefile -> closefile != null)
				.map(closeFile -> closeFile.getFileCall())
				.filter(call -> Call.CallType.FILE_CONTROL_ENTRY_CALL.equals(call.getCallType()))
				.map(call -> (FileControlEntryCall) call).filter(fileCall -> fileCall != null)
				.map(fileCall -> fileCall.getFileControlEntry()).filter(fileControlEntry -> fileControlEntry != null)
				.map(fileControlEntry -> fileControlEntry.getFileDescriptionEntry()).distinct();
	}

	protected Stream<FileDescriptionEntry> getOpenedFiles(final OpenStatement openStatement) {
		final List<Call> calls = new ArrayList<Call>();
		final Stream<ExtendPhrase> extendPhrases = openStatement.getExtendPhrases().stream()
				.filter(phrase -> phrase != null);
		final Stream<InputOutputPhrase> inputOutputPhrases = openStatement.getInputOutputPhrases().stream()
				.filter(phrase -> phrase != null);
		final Stream<InputPhrase> inputPhrases = openStatement.getInputPhrases().stream()
				.filter(phrase -> phrase != null);
		final Stream<OutputPhrase> outputPhrases = openStatement.getOutputPhrases().stream()
				.filter(phrase -> phrase != null);

		calls.addAll(extendPhrases.flatMap(extendPhrase -> extendPhrase.getFileCalls().stream())
				.collect(Collectors.toList()));
		calls.addAll(inputOutputPhrases.flatMap(phrase -> phrase.getFileCalls().stream()).collect(Collectors.toList()));
		calls.addAll(inputPhrases.flatMap(phrase -> phrase.getInputs().stream()).map(input -> input.getFileCall())
				.collect(Collectors.toList()));
		calls.addAll(outputPhrases.flatMap(phrase -> phrase.getOutputs().stream()).map(output -> output.getFileCall())
				.collect(Collectors.toList()));

		return calls.stream().filter(call -> call != null).filter(call -> {
			return Call.CallType.FILE_CONTROL_ENTRY_CALL.equals(call.getCallType());
		}).map(call -> (FileControlEntryCall) call).map(fileCall -> {
			return fileCall.getFileControlEntry();
		}).filter(fileControlEntry -> fileControlEntry != null).map(fileControlEntry -> {
			return fileControlEntry.getFileDescriptionEntry();
		}).distinct();

	}

	protected boolean isRelevantStatement(final OpenStatement openStatement,
			final List<FileDescriptionEntry> closedFiles) {
		final List<FileDescriptionEntry> openedFiles = getOpenedFiles(openStatement)
				.filter(file -> !closedFiles.contains(file)).collect(Collectors.toList());

		if (openedFiles == null || openedFiles.size() == 0) {
			return false;
		}
		return true;
	}
}
