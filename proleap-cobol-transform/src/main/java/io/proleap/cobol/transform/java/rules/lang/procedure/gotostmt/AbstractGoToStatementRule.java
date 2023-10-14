package io.proleap.cobol.transform.java.rules.lang.procedure.gotostmt;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import org.antlr.v4.runtime.ParserRuleContext;

import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.ProcedureCall;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.transform.java.identifier.JavaIdentifierService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public abstract class AbstractGoToStatementRule<S extends ParserRuleContext, T extends ASGElement>
		extends CobolTransformRule<S, T> {

	@Inject
	private JavaIdentifierService javaIdentifierService;

	protected void printCall(final Call call, final boolean exception, final RuleContext rc) {
		final CallType callType = call.getCallType();

		switch (callType) {
		case PROCEDURE_CALL:
			final ProcedureCall procedureCall = (ProcedureCall) call;
			printProcedureCall(procedureCall, exception, rc);
			break;
		default:
			break;
		}
	}

	protected void printProcedureCall(final ProcedureCall procedureCall, final boolean exception,
			final RuleContext rc) {
		final Paragraph paragraph = procedureCall.getParagraph();
		final String methodName = javaIdentifierService.mapToIdentifier(paragraph.getName());

		rc.p("%s();", methodName);
		rc.pNl(procedureCall);

		if (exception) {
			rc.p("throw new RuntimeException(\"%s must terminate due to GO TO in call stack.\");", methodName);
			rc.pNl();
		}
	}
}
