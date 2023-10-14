package io.proleap.cobol.analysis.issues.rules.procedure.accept;

import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.accept.AcceptStatement;
import io.proleap.cobol.asg.metamodel.procedure.accept.AcceptStatement.AcceptType;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class AcceptShouldNotBeUsedFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final boolean isAcceptStatement = StatementTypeEnum.ACCEPT.equals(statement.getStatementType());
		final boolean result;

		if (!isAcceptStatement) {
			result = false;
		} else {
			final AcceptStatement acceptStatement = (AcceptStatement) statement;
			result = !AcceptType.DATE.equals(acceptStatement.getAcceptType());
		}

		return result;
	}
}
