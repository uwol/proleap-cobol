package io.proleap.cobol.analysis.util;

import java.util.ArrayList;
import java.util.List;

import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.IfStatement;
import io.proleap.cobol.asg.metamodel.procedure.search.SearchStatement;
import io.proleap.cobol.asg.metamodel.valuestmt.ConditionValueStmt;

public class StatementUtils {

	public static boolean containsCondition(final Statement statement) {
		final StatementTypeEnum statementType = (StatementTypeEnum) statement.getStatementType();
		final boolean result;

		switch (statementType) {
		case IF:
		case SEARCH:
			result = true;
			break;
		default:
			result = false;
			break;
		}

		return result;
	}

	public static List<ConditionValueStmt> getConditions(final Statement statement) {
		final StatementTypeEnum statementType = (StatementTypeEnum) statement.getStatementType();
		final List<ConditionValueStmt> result = new ArrayList<>();

		switch (statementType) {
		case IF:
			final IfStatement ifStatement = (IfStatement) statement;
			result.add(ifStatement.getCondition());
			break;
		case SEARCH:
			final SearchStatement search = ((SearchStatement) statement);

			search.getWhenPhrases().forEach(phrase -> {
				result.add(phrase.getCondition());
			});
			break;
		default:
			break;
		}

		return result;
	}
}
