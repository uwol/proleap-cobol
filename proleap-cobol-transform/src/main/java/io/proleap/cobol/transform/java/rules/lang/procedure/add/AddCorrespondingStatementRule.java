package io.proleap.cobol.transform.java.rules.lang.procedure.add;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.AddCorrespondingStatementContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.procedure.add.AddCorrespondingStatement;
import io.proleap.cobol.asg.metamodel.procedure.add.To;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class AddCorrespondingStatementRule
		extends CobolTransformRule<AddCorrespondingStatementContext, AddCorrespondingStatement> {

	@Override
	public void apply(final AddCorrespondingStatementContext ctx,
			final AddCorrespondingStatement addCorrespondingStatement, final RuleContext rc) {
		final Call from = addCorrespondingStatement.getFromCall();
		final To to = addCorrespondingStatement.getTo();

		printCorresponding(from, to, rc);
	}

	@Override
	public Class<AddCorrespondingStatementContext> from() {
		return AddCorrespondingStatementContext.class;
	}

	protected void printCorresponding(final Call from, final To to, final RuleContext rc) {
		rc.visit(to.getToCall().getCtx());
		rc.p(".add(");
		rc.visit(from.getCtx());
		rc.p(");");
		rc.pNl(from);
	}
}
