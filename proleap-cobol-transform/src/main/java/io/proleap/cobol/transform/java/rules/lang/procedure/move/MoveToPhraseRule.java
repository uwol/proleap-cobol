package io.proleap.cobol.transform.java.rules.lang.procedure.move;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.MoveToStatementContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveToSendingArea;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveToStatement;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.type.CobolTypeService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class MoveToPhraseRule extends CobolTransformRule<MoveToStatementContext, MoveToStatement> {

	@Inject
	private CobolTypeService cobolTypeService;

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Override
	public void apply(final MoveToStatementContext ctx, final MoveToStatement moveToPhrase, final RuleContext rc) {
		final MoveToSendingArea sendingArea = moveToPhrase.getSendingArea();

		for (final Call call : moveToPhrase.getReceivingAreaCalls()) {
			final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService.getDataDescriptionEntry(call);

			if (dataDescriptionEntry == null) {
				printScalarMoveTo(call, sendingArea, rc);
			} else {
				final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry
						.getDataDescriptionEntryType();

				if (!DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)
						|| !dataDescriptionEntryService.hasChildren(dataDescriptionEntry)) {
					printScalarMoveTo(call, sendingArea, rc);
				} else {
					printGroupMoveTo(call, sendingArea, rc);
				}
			}
		}
	}

	@Override
	public Class<MoveToStatementContext> from() {
		return MoveToStatementContext.class;
	}

	protected void printGroupMoveTo(final Call call, final MoveToSendingArea sendingArea, final RuleContext rc) {
		rc.p("entityService.assignTo(");
		rc.visit(call.getCtx());
		rc.p(", ");
		rc.visit(sendingArea.getSendingAreaValueStmt().getCtx());
		rc.p(");");
		rc.pNl(call);
	}

	protected void printScalarMoveTo(final Call call, final MoveToSendingArea sendingArea, final RuleContext rc) {
		rc.visit(call.getCtx());
		rc.p(" = ");
		rc.getTypedPrinter().printWithAdjustedType(sendingArea.getSendingAreaValueStmt().getCtx(),
				cobolTypeService.getType(sendingArea.getSendingAreaValueStmt()), cobolTypeService.getType(call));
		rc.p(";");
		rc.pNl(call);
	}
}
