package io.proleap.cobol.transform.java.rules.lang.procedure.subtract;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.SubtractStatementContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.procedure.subtract.Giving;
import io.proleap.cobol.asg.metamodel.procedure.subtract.Minuend;
import io.proleap.cobol.asg.metamodel.procedure.subtract.MinuendGiving;
import io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractFromGivingStatement;
import io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractFromStatement;
import io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractStatement;
import io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractStatement.SubtractType;
import io.proleap.cobol.asg.metamodel.procedure.subtract.Subtrahend;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class SubtractStatementRule extends CobolTransformRule<SubtractStatementContext, SubtractStatement> {

	@Override
	public void apply(final SubtractStatementContext ctx, final SubtractStatement subtractStatement,
			final RuleContext rc) {
		final SubtractType type = subtractStatement.getSubtractType();

		switch (type) {
		case CORRESPONDING:
			printCorresponding(subtractStatement, rc);
			break;
		case FROM:
			printSubtractFrom(subtractStatement, rc);
			break;
		case FROM_GIVING:
			printSubtractFromGiving(subtractStatement, rc);
			break;
		default:
			break;
		}
	}

	@Override
	public Class<SubtractStatementContext> from() {
		return SubtractStatementContext.class;
	}

	protected void printCorresponding(final SubtractStatement subtractStatement, final RuleContext rc) {

	}

	protected void printGiving(final RuleContext rc, final SubtractFromGivingStatement subtractFromGiving,
			final Giving giving) {
		final Call givingCall = giving.getGivingCall();

		if (givingCall != null) {
			rc.visit(givingCall.getCtx());
		}

		rc.p(" = ");

		final MinuendGiving minuend = subtractFromGiving.getMinuend();

		if (minuend != null) {
			final ValueStmt minuendValueStmt = minuend.getMinuendValueStmt();

			if (minuendValueStmt != null) {
				rc.visit(minuendValueStmt.getCtx());
			}
		}

		for (final Subtrahend subtrahend : subtractFromGiving.getSubtrahends()) {
			rc.p(".subtract(");

			final ValueStmt subtrahendValueStmt = subtrahend.getSubtrahendValueStmt();

			if (subtrahendValueStmt != null) {
				rc.visit(subtrahendValueStmt.getCtx());
			}

			rc.p(")");
		}

		rc.p(";");
		rc.pNl(giving);

	}

	protected void printSubtractFrom(final SubtractStatement subtractStatement, final RuleContext rc) {
		final SubtractFromStatement from = subtractStatement.getSubtractFromStatement();

		for (final Minuend minuend : from.getMinuends()) {
			final Call minuendCall = minuend.getMinuendCall();

			if (minuendCall != null) {
				rc.visit(minuendCall.getCtx());
			}

			rc.p(" = ");

			if (minuendCall != null) {
				rc.visit(minuendCall.getCtx());
			}

			for (final Subtrahend subtrahend : from.getSubtrahends()) {
				rc.p(".subtract(");

				final ValueStmt subtrahendValueStmt = subtrahend.getSubtrahendValueStmt();

				if (subtrahendValueStmt != null) {
					rc.visit(subtrahendValueStmt.getCtx());
				}

				rc.p(")");
			}

			rc.p(";");
			rc.pNl(minuend);
		}
	}

	protected void printSubtractFromGiving(final SubtractStatement subtractStatement, final RuleContext rc) {
		final SubtractFromGivingStatement subtractFromGiving = subtractStatement.getSubtractFromGivingStatement();

		for (final Giving giving : subtractFromGiving.getGivings()) {
			printGiving(rc, subtractFromGiving, giving);
		}
	}
}
