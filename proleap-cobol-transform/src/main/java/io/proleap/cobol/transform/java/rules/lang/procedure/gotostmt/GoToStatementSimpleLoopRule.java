package io.proleap.cobol.transform.java.rules.lang.procedure.gotostmt;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.GoToStatementSimpleContext;
import io.proleap.cobol.asg.metamodel.Scope;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.ProcedureCall;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.GoToStatement;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.Simple;
import io.proleap.cobol.asg.util.ANTLRUtils;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class GoToStatementSimpleLoopRule extends AbstractGoToStatementRule<GoToStatementSimpleContext, Simple> {

	@Override
	public void apply(final GoToStatementSimpleContext ctx, final Simple simple, final RuleContext rc) {
		final Call call = simple.getProcedureCall();
		printCall(call, false, rc);
	}

	@Override
	public Class<GoToStatementSimpleContext> from() {
		return GoToStatementSimpleContext.class;
	}

	@Override
	public Integer getPriority() {
		return 256;
	}

	@Override
	public boolean where(final GoToStatementSimpleContext ctx, final Simple simple, final RuleContext rc) {
		final boolean result;

		final GoToStatement goToStatement = (GoToStatement) ANTLRUtils.findParent(GoToStatement.class, ctx,
				simple.getProgram().getASGElementRegistry());

		if (goToStatement == null) {
			result = false;
		} else {
			final Scope scope = goToStatement.getScope();

			final Call call = simple.getProcedureCall();
			final CallType callType = call.getCallType();

			switch (callType) {
			case PROCEDURE_CALL:
				final ProcedureCall procedureCall = (ProcedureCall) call;
				final Paragraph paragraph = procedureCall.getParagraph();

				result = paragraph.equals(scope);
				break;
			default:
				result = false;
				break;
			}
		}

		return result;
	}
}
