package io.proleap.cobol.analysis.issues.rules.procedure.ifstmt;

import java.util.List;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.IfStatement;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.Then;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class CollapsableIfStatementFeatureGenerator extends FeatureGenerator<Statement> {

	protected boolean containsOnlyOneIfStatement(final List<Statement> statements) {
		if (statements != null && statements.size() == 1) {
			return StatementTypeEnum.IF.equals(statements.get(0).getStatementType());
		}
		return false;
	}

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {

		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return statement != null && statement.getStatementType().equals(StatementTypeEnum.IF);
		}).filter(statement -> isRelevantStatement(statement));

	}

	protected boolean isRelevantIfStatement(final Statement statement) {
		final IfStatement ifStatement = (IfStatement) statement;
		final Then then = ifStatement.getThen();
		final List<Statement> statements = then.getStatements();

		return containsOnlyOneIfStatement(statements);

	}

	protected boolean isRelevantStatement(final Statement statement) {
		if (statement == null || !StatementTypeEnum.IF.equals(statement.getStatementType())) {
			return false;
		}

		return isRelevantIfStatement(statement);
	}
}
