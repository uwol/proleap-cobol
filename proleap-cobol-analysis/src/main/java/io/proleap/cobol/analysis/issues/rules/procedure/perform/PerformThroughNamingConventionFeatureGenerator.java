package io.proleap.cobol.analysis.issues.rules.procedure.perform;

import java.util.List;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.ProcedureCall;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformProcedureStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement.PerformStatementType;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class PerformThroughNamingConventionFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isRelevantPerformProcedureStatement(final PerformStatement performStatement) {
		final boolean result;
		final PerformProcedureStatement performProcedureStatement = performStatement.getPerformProcedureStatement();

		final List<Call> calls = performProcedureStatement.getCalls();
		final int size = calls.size();

		if (size < 2) {
			result = false;
		} else {
			final Call lastCall = calls.get(size - 1);
			final CallType lastCallType = lastCall.getCallType();
			final Call firstCall = calls.get(0);
			final CallType firstCallType = firstCall.getCallType();

			if (!CallType.PROCEDURE_CALL.equals(lastCallType) || !CallType.PROCEDURE_CALL.equals(firstCallType)) {
				result = false;
			} else {
				final ProcedureCall lastProcedureCall = (ProcedureCall) lastCall;
				final ProcedureCall firstProcedureCall = (ProcedureCall) firstCall;
				if (lastProcedureCall.getParagraph() == null || firstProcedureCall.getParagraph() == null) {
					return false;
				}
				final Paragraph lastParagraph = lastProcedureCall.getParagraph();
				final Paragraph firstParagraph = firstProcedureCall.getParagraph();

				final String firstName = firstParagraph.getName();
				final String lastName = lastParagraph.getName();

				return !isSharingNamingConvention(firstName, lastName);

			}
		}

		return result;
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final boolean result;
		final StatementType statementType = statement.getStatementType();

		if (!StatementTypeEnum.PERFORM.equals(statementType)) {
			result = false;
		} else {
			final PerformStatement performStatement = (PerformStatement) statement;
			final PerformStatementType performStatementType = performStatement.getPerformStatementType();

			if (!PerformStatementType.PROCEDURE.equals(performStatementType)) {
				result = false;
			} else {
				result = isRelevantPerformProcedureStatement(performStatement);
			}
		}
		return result;
	}

	protected boolean isSharingNamingConvention(final String first, final String last) {
		if (last.endsWith(first) || last.startsWith(first)) {
			return true;
		}
		return false;
	}
}
