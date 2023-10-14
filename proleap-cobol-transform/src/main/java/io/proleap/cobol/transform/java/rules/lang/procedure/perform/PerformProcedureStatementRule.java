package io.proleap.cobol.transform.java.rules.lang.procedure.perform;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.PerformProcedureStatementContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformProcedureStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformType;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformType.PerformTypeType;
import io.proleap.cobol.asg.metamodel.procedure.perform.TestClause;
import io.proleap.cobol.asg.metamodel.procedure.perform.TestClause.TestClauseType;
import io.proleap.cobol.asg.metamodel.procedure.perform.Times;
import io.proleap.cobol.asg.metamodel.procedure.perform.Until;
import io.proleap.cobol.asg.metamodel.procedure.perform.Varying;
import io.proleap.cobol.transform.java.identifier.JavaIdentifierService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class PerformProcedureStatementRule
		extends CobolTransformRule<PerformProcedureStatementContext, PerformProcedureStatement> {

	@Inject
	private JavaIdentifierService javaIdentifierService;

	@Override
	public void apply(final PerformProcedureStatementContext ctx,
			final PerformProcedureStatement performProcedureStatement, final RuleContext rc) {
		final PerformType performType = performProcedureStatement.getPerformType();

		if (performType == null) {
			visitCalls(performProcedureStatement, rc);
		} else {
			final PerformTypeType type = performType.getPerformTypeType();

			switch (type) {
			case TIMES:
				final Times times = performType.getTimes();
				printTimes(performProcedureStatement, times, rc);
				break;
			case UNTIL:
				final Until until = performType.getUntil();
				printUntil(performProcedureStatement, until, rc);
				break;
			case VARYING:
				final Varying varying = performType.getVarying();
				printVarying(performProcedureStatement, varying, rc);
				break;
			default:
				break;
			}

			rc.pNl(performProcedureStatement);
		}
	}

	@Override
	public Class<PerformProcedureStatementContext> from() {
		return PerformProcedureStatementContext.class;
	}

	protected void printTimes(final PerformProcedureStatement performProcedureStatement, final Times times,
			final RuleContext rc) {
		rc.p("for(BigDecimal i=BigDecimal.ZERO; i.compareTo(");
		rc.visit(times.getTimesValueStmt().getCtx());
		rc.p(") < 0; i = i.add(BigDecimal.ONE)){");
		rc.pNl();

		rc.getPrinter().indent();
		visitCalls(performProcedureStatement, rc);
		rc.getPrinter().unindent();

		rc.p("}");

		rc.pNl(times);
	}

	protected void printUntil(final PerformProcedureStatement performProcedureStatement, final Until until,
			final RuleContext rc) {
		final TestClause testClause = until.getTestClause();

		if (testClause != null && TestClauseType.AFTER.equals(testClause.getTestClauseType())) {
			rc.p("do {");
			rc.pNl();

			rc.getPrinter().indent();
			visitCalls(performProcedureStatement, rc);
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
			visitCalls(performProcedureStatement, rc);
			rc.getPrinter().unindent();

			rc.p("}");
			rc.pNl();
		}
	}

	protected void printVarying(final PerformProcedureStatement performProcedureStatement, final Varying varying,
			final RuleContext rc) {

	}

	protected void visitCalls(final PerformProcedureStatement performProcedureStatement, final RuleContext rc) {
		for (final Call call : performProcedureStatement.getCalls()) {
			rc.p("%s();", javaIdentifierService.mapToIdentifier(call.getName()));
			rc.pNl(performProcedureStatement);
		}
	}
}
