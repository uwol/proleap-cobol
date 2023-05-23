package io.proleap.cobol.analysis.issues.rules.procedure.perform;

import java.util.List;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformInlineStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement.PerformStatementType;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformType;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class OpenShouldNotBeUsedInLoopFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final StatementType statementType = statement.getStatementType();
		final boolean result;

		if (!StatementTypeEnum.PERFORM.equals(statementType)) {
			result = false;
		} else {
			final PerformStatement performStatement = (PerformStatement) statement;
			final PerformStatementType performStatementType = performStatement.getPerformStatementType();

			if (!PerformStatementType.INLINE.equals(performStatementType)) {
				result = false;
			} else {
				final PerformInlineStatement performInlineStatement = performStatement.getPerformInlineStatement();
				final PerformType performType = performInlineStatement.getPerformType();

				if (performType == null) {
					result = false;
				} else {
					final List<Statement> inLineStatements = performInlineStatement.getStatements();

					inLineStatements.removeIf(
							inLineStatement -> !StatementTypeEnum.OPEN.equals(inLineStatement.getStatementType()));

					if (inLineStatements.isEmpty()) {
						result = false;
					} else {
						result = true;
					}
				}
			}
		}

		return result;
	}
}
