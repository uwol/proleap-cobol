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
public class PerformThroughLastParagraphShouldOnlyContainExitFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isRelevantPerformProcedureStatement(final PerformProcedureStatement performProcedureStatement) {
		final boolean result;

		final List<Call> calls = performProcedureStatement.getCalls();
		final int size = calls.size();

		if (size < 2) {
			result = false;
		} else {
			final Call call = calls.get(size - 1);
			final CallType callType = call.getCallType();

			if (!CallType.PROCEDURE_CALL.equals(callType)) {
				result = false;
			} else {
				final ProcedureCall procedureCall = (ProcedureCall) call;
				final Paragraph paragraph = procedureCall.getParagraph();
				final List<Statement> statements = paragraph.getStatements();

				if (statements.size() == 1) {
					final Statement statement = statements.get(0);
					final StatementType statementType = statement.getStatementType();

					if (StatementTypeEnum.EXIT.equals(statementType)) {
						result = false;
					} else {
						result = true;
					}
				} else {
					result = true;
				}
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
				final PerformProcedureStatement performProcedureStatement = performStatement
						.getPerformProcedureStatement();

				result = isRelevantPerformProcedureStatement(performProcedureStatement);
			}
		}

		return result;
	}
}
