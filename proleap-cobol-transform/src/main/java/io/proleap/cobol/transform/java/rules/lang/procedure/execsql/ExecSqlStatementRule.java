package io.proleap.cobol.transform.java.rules.lang.procedure.execsql;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ExecSqlStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.execsql.ExecSqlStatement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ExecSqlStatementRule extends CobolTransformRule<ExecSqlStatementContext, ExecSqlStatement> {

	@Override
	public void apply(final ExecSqlStatementContext ctx, final ExecSqlStatement execSqlStatement,
			final RuleContext rc) {
		final String execSqlText = execSqlStatement.getExecSqlText();

		rc.p("java.sql.DriverManager.getConnection(\"\").prepareStatement(");
		rc.p("\"");
		rc.p(execSqlText);
		rc.p("\"");
		rc.p(").executeQuery();");

		rc.pNl(execSqlStatement);
	}

	@Override
	public Class<ExecSqlStatementContext> from() {
		return ExecSqlStatementContext.class;
	}
}
