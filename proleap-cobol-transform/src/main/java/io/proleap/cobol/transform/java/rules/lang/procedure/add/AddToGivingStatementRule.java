package io.proleap.cobol.transform.java.rules.lang.procedure.add;

import java.util.List;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.AddToGivingStatementContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.procedure.add.AddToGivingStatement;
import io.proleap.cobol.asg.metamodel.procedure.add.From;
import io.proleap.cobol.asg.metamodel.procedure.add.Giving;
import io.proleap.cobol.asg.metamodel.procedure.add.ToGiving;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class AddToGivingStatementRule extends CobolTransformRule<AddToGivingStatementContext, AddToGivingStatement> {

	@Override
	public void apply(final AddToGivingStatementContext ctx, final AddToGivingStatement addToGivingStatement,
			final RuleContext rc) {
		final List<From> froms = addToGivingStatement.getFroms();
		final List<ToGiving> tos = addToGivingStatement.getTos();
		final List<Giving> givings = addToGivingStatement.getGivings();

		for (final Giving giving : givings) {
			printGiving(froms, tos, giving, rc);
		}
	}

	@Override
	public Class<AddToGivingStatementContext> from() {
		return AddToGivingStatementContext.class;
	}

	protected void printGiving(final List<From> froms, final List<ToGiving> tos, final Giving giving,
			final RuleContext rc) {
		final Call givingGiving = giving.getGivingCall();

		if (givingGiving != null) {
			rc.visit(givingGiving.getCtx());
		}

		rc.p(" = ");

		boolean firstTo = true;

		for (final ToGiving to : tos) {
			if (firstTo) {
				rc.visit(to.getToValueStmt().getCtx());
			} else {
				rc.p(".add(");
				rc.visit(to.getToValueStmt().getCtx());
				rc.p(")");
			}

			firstTo = false;
		}

		for (final From from : froms) {
			rc.p(".add(");
			rc.visit(from.getFromValueStmt().getCtx());
			rc.p(")");
		}

		rc.p(";");
		rc.pNl(giving);
	}
}
