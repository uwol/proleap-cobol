package io.proleap.cobol.analysis.issues.rules.procedure.perform;

import java.util.List;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformProcedureStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement.PerformStatementType;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class PerformSectionShouldNotBeUsedFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isRelevantPerformProcedureStatement(final PerformStatement performStatement) {
		final PerformProcedureStatement performProcedureStatement = performStatement.getPerformProcedureStatement();
		final List<Call> calls = performProcedureStatement.getCalls();

		for (final Call call : calls) {
			final CallType callType = call.getCallType();
			if (CallType.SECTION_CALL.equals(callType)) {
				return true;
			}
		}

		return false;
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
}
