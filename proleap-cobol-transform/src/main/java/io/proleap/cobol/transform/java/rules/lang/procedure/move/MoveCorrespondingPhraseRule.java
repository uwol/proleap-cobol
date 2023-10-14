package io.proleap.cobol.transform.java.rules.lang.procedure.move;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.MoveCorrespondingToStatementContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveCorrespondingToSendingArea;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveCorrespondingToStatetement;
import io.proleap.cobol.commons.type.CobolTypeService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class MoveCorrespondingPhraseRule
		extends CobolTransformRule<MoveCorrespondingToStatementContext, MoveCorrespondingToStatetement> {

	@Inject
	private CobolTypeService cobolTypeService;

	@Override
	public void apply(final MoveCorrespondingToStatementContext ctx,
			final MoveCorrespondingToStatetement moveCorrespondingPhrase, final RuleContext rc) {
		final MoveCorrespondingToSendingArea sendingArea = moveCorrespondingPhrase.getMoveToCorrespondingSendingArea();
		final Call sendingCall = sendingArea.getSendingAreaCall();

		for (final Call call : moveCorrespondingPhrase.getReceivingAreaCalls()) {
			rc.visit(call.getCtx());
			rc.p(" = ");
			rc.getTypedPrinter().printWithAdjustedType(sendingCall.getCtx(), cobolTypeService.getType(sendingCall),
					cobolTypeService.getType(call));
			rc.p(";");
			rc.pNl(call);
		}
	}

	@Override
	public Class<MoveCorrespondingToStatementContext> from() {
		return MoveCorrespondingToStatementContext.class;
	}
}
