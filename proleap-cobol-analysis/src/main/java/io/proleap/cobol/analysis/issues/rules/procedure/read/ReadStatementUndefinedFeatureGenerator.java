package io.proleap.cobol.analysis.issues.rules.procedure.read;

import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.FileControlEntryCall;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileStatusClause;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.read.ReadStatement;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class ReadStatementUndefinedFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean hasAtEndPhrase(final ReadStatement readStatement) {
		return readStatement.getAtEnd() != null;
	}

	protected boolean hasInvalidKeyPhrase(final ReadStatement readStatement) {
		return readStatement.getInvalidKeyPhrase() != null;
	}

	protected boolean isRelevantReadStatement(final ReadStatement readStatement) {
		final Call call = readStatement.getFileCall();

		if (CallType.FILE_CONTROL_ENTRY_CALL.equals(call.getCallType())) {
			final FileControlEntryCall fileControlEntrycall = (FileControlEntryCall) call;
			final FileControlEntry fileControlEntry = fileControlEntrycall.getFileControlEntry();
			final FileStatusClause fileStatusClause = fileControlEntry.getFileStatusClause();

			if (fileStatusClause == null) {
				if (!hasAtEndPhrase(readStatement) && !hasInvalidKeyPhrase(readStatement)) {
					return true;
				}
			}
		}

		return false;
	}

	protected boolean isRelevantStatement(final Statement statement) {
		if (statement.getStatementType().equals(StatementTypeEnum.READ)) {
			final ReadStatement readStatement = (ReadStatement) statement;
			return isRelevantReadStatement(readStatement);
		}

		return false;
	}
}
