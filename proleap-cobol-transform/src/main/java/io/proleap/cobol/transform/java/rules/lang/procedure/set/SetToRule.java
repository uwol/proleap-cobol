package io.proleap.cobol.transform.java.rules.lang.procedure.set;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.SetToStatementContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.procedure.set.SetTo;
import io.proleap.cobol.asg.metamodel.procedure.set.To;
import io.proleap.cobol.asg.metamodel.procedure.set.Value;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.type.CobolTypeService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class SetToRule extends CobolTransformRule<SetToStatementContext, SetTo> {

	@Inject
	private CobolTypeService cobolTypeService;

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Override
	public void apply(final SetToStatementContext ctx, final SetTo setTo, final RuleContext rc) {
		for (final To to : setTo.getTos()) {
			for (final Value value : setTo.getValues()) {
				final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
						.getDataDescriptionEntry(to.getToCall());

				if (dataDescriptionEntry == null) {
					printScalarSetTo(to.getToCall(), value, rc);
				} else {
					final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry
							.getDataDescriptionEntryType();

					if (!DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)
							|| !dataDescriptionEntryService.hasChildren(dataDescriptionEntry)) {
						printScalarSetTo(to.getToCall(), value, rc);
					} else {
						printGroupSetTo(to.getToCall(), value, rc);
					}
				}
			}
		}
	}

	@Override
	public Class<SetToStatementContext> from() {
		return SetToStatementContext.class;
	}

	protected void printGroupSetTo(final Call call, final Value value, final RuleContext rc) {
		rc.p("entityService.assignTo(");
		rc.visit(call.getCtx());
		rc.p(", ");
		rc.visit(value.getValueStmt().getCtx());
		rc.p(");");
		rc.pNl(call);
	}

	protected void printScalarSetTo(final Call call, final Value value, final RuleContext rc) {
		rc.visit(call.getCtx());
		rc.p(" = ");
		rc.getTypedPrinter().printWithAdjustedType(value.getValueStmt().getCtx(),
				cobolTypeService.getType(value.getValueStmt()), cobolTypeService.getType(call));
		rc.p(";");
		rc.pNl(call);
	}
}
