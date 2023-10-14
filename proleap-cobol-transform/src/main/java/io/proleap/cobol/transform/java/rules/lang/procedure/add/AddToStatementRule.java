package io.proleap.cobol.transform.java.rules.lang.procedure.add;

import java.util.List;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.AddToStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.add.AddToStatement;
import io.proleap.cobol.asg.metamodel.procedure.add.From;
import io.proleap.cobol.asg.metamodel.procedure.add.To;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class AddToStatementRule extends CobolTransformRule<AddToStatementContext, AddToStatement> {

	@Override
	public void apply(final AddToStatementContext ctx, final AddToStatement addToStatement, final RuleContext rc) {
		final List<From> froms = addToStatement.getFroms();
		final List<To> tos = addToStatement.getTos();

		for (final From from : froms) {
			for (final To to : tos) {
				printTo(from, to, rc);
			}
		}
	}

	@Override
	public Class<AddToStatementContext> from() {
		return AddToStatementContext.class;
	}

	protected void printTo(final From from, final To to, final RuleContext rc) {
		rc.visit(to.getToCall().getCtx());
		rc.p(" = ");
		rc.visit(to.getToCall().getCtx());
		rc.p(".add(");
		rc.visit(from.getFromValueStmt().getCtx());
		rc.p(");");
		rc.pNl(from);
	}
}
