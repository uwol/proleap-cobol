package io.proleap.cobol.analysis.issues.rules.procedure.perform;

import java.util.List;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformProcedureStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement.PerformStatementType;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class PerformThroughShouldNotBeUsedFeatureGenerator extends FeatureGenerator<Statement> {

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
			result = true;
		}

		return result;
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final StatementType statementType = statement.getStatementType();
		final boolean result;

		if (!StatementTypeEnum.PERFORM.equals(statementType)) {
			result = false;
		} else {
			final PerformStatement performStatement = (PerformStatement) statement;
			final PerformStatementType performStatementType = performStatement.getPerformStatementType();

			if (PerformStatementType.PROCEDURE.equals(performStatementType)) {
				final PerformProcedureStatement performProcedureStatement = performStatement
						.getPerformProcedureStatement();
				result = isRelevantPerformProcedureStatement(performProcedureStatement);
			} else {
				result = false;
			}
		}

		return result;
	}
}
