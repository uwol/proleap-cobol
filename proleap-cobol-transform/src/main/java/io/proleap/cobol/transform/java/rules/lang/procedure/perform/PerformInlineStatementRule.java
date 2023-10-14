package io.proleap.cobol.transform.java.rules.lang.procedure.perform;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.PerformInlineStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformInlineStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformType;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformType.PerformTypeType;
import io.proleap.cobol.asg.metamodel.procedure.perform.TestClause;
import io.proleap.cobol.asg.metamodel.procedure.perform.Times;
import io.proleap.cobol.asg.metamodel.procedure.perform.Until;
import io.proleap.cobol.asg.metamodel.procedure.perform.Varying;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class PerformInlineStatementRule extends CobolTransformRule<PerformInlineStatementContext, PerformInlineStatement> {

	@Override
	public void apply(final PerformInlineStatementContext ctx, final PerformInlineStatement performInlineStatement,
			final RuleContext rc) {
		final PerformType performType = performInlineStatement.getPerformType();

		if (performType == null) {
			visitStatements(performInlineStatement, rc);
		} else {
			final PerformTypeType type = performType.getPerformTypeType();

			if (type == null) {
			} else {
				switch (type) {
				case TIMES:
					final Times times = performType.getTimes();
					printTimes(performInlineStatement, times, rc);
					break;
				case UNTIL:
					final Until until = performType.getUntil();
					printUntil(performInlineStatement, until, rc);
					break;
				case VARYING:
					final Varying varying = performType.getVarying();
					printVarying(performInlineStatement, varying, rc);
					break;
				default:
					break;
				}
			}
		}
	}

	@Override
	public Class<PerformInlineStatementContext> from() {
		return PerformInlineStatementContext.class;
	}

	protected void printTimes(final PerformInlineStatement performInlineStatement, final Times times,
			final RuleContext rc) {
		rc.pNl();
		rc.p("for(BigDecimal i=BigDecimal.ZERO; i.compareTo(");
		rc.visit(times.getTimesValueStmt().getCtx());
		rc.p(") < 0; i = i.add(BigDecimal.ONE)){");
		rc.pNl(times);

		rc.getPrinter().indent();
		visitStatements(performInlineStatement, rc);
		rc.getPrinter().unindent();

		rc.p("}");
		rc.pNl();
		rc.pNl();
	}

	protected void printUntil(final PerformInlineStatement performInlineStatement, final Until until,
			final RuleContext rc) {
		final TestClause testClause = until.getTestClause();

		rc.pNl();

		if (testClause != null && TestClause.TestClauseType.AFTER.equals(testClause.getTestClauseType())) {
			rc.p("do {");
			rc.pNl();

			rc.getPrinter().indent();
			visitStatements(performInlineStatement, rc);
			rc.getPrinter().unindent();

			rc.p("} while(!(");
			rc.visit(until.getCondition().getCtx());
			rc.p("));");
			rc.pNl(until);
		} else {
			rc.p("while(!(");
			rc.visit(until.getCondition().getCtx());
			rc.p(")){");
			rc.pNl(until);

			rc.getPrinter().indent();
			visitStatements(performInlineStatement, rc);
			rc.getPrinter().unindent();

			rc.p("}");
			rc.pNl();
		}
	}

	protected void printVarying(final PerformInlineStatement performInlineStatement, final Varying varying,
			final RuleContext rc) {
		visitStatements(performInlineStatement, rc);
	}

	private void visitStatements(final PerformInlineStatement performInlineStatement, final RuleContext rc) {
		for (final Statement statement : performInlineStatement.getStatements()) {
			rc.visit(statement.getCtx());
		}
	}
}
