package io.proleap.cobol.analysis.issues.rules.procedure.evaluate;

import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.EvaluateStatement;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.WhenOther;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class EvaluateShouldEndWithWhenOtherFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final boolean result;
		final StatementType statementType = statement.getStatementType();

		if (!StatementTypeEnum.EVALUATE.equals(statementType)) {
			result = false;
		} else {
			final EvaluateStatement evalStmt = (EvaluateStatement) statement;
			final WhenOther whenOther = evalStmt.getWhenOther();

			if (whenOther == null) {
				result = false;
			} else {
				result = true;
			}
		}

		return result;
	}
}
