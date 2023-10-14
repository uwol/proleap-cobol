package io.proleap.cobol.transform.java.rules.lang.procedure.stop;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.StopStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.stop.StopStatement;
import io.proleap.cobol.asg.metamodel.procedure.stop.StopStatement.StopType;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class StopStatementRule extends CobolTransformRule<StopStatementContext, StopStatement> {

	@Override
	public void apply(final StopStatementContext ctx, final StopStatement stopStatement, final RuleContext rc) {
		final StopType type = stopStatement.getStopType();

		switch (type) {
		case STOP_RUN:
			stopRun(stopStatement, rc);
			break;
		case STOP_RUN_AND_DISPLAY:
			stopRunAndDisplay(stopStatement, rc);
			break;
		default:
			break;
		}
	}

	@Override
	public Class<StopStatementContext> from() {
		return StopStatementContext.class;
	}

	private void stopRun(final StopStatement stopStatement, final RuleContext rc) {
		rc.p("System.exit(0);");
		rc.pNl(stopStatement);
	}

	protected void stopRunAndDisplay(final StopStatement stopStatement, final RuleContext rc) {
		rc.p("System.out.println(");
		rc.visit(stopStatement.getDisplayValueStmt().getCtx());
		rc.p(");");
		rc.pNl(stopStatement);

		stopRun(stopStatement, rc);
	}
}
