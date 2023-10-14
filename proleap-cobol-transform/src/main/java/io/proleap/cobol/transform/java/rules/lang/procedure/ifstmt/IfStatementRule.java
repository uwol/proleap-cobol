package io.proleap.cobol.transform.java.rules.lang.procedure.ifstmt;

import java.util.List;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.IfStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.Else;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.IfStatement;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.Then;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class IfStatementRule extends CobolTransformRule<IfStatementContext, IfStatement> {

	@Override
	public void apply(final IfStatementContext ctx, final IfStatement ifStatement, final RuleContext rc) {
		rc.p("if (");
		rc.visit(ifStatement.getCondition().getCtx());
		rc.p(") {");
		rc.pNl(ifStatement);

		// then
		rc.getPrinter().indent();
		final Then then = ifStatement.getThen();
		visitStatements(then.getStatements(), rc);

		rc.getPrinter().unindent();
		rc.p("} ");

		// else
		final Else elseBlock = ifStatement.getElse();

		if (elseBlock != null) {
			rc.p("else {");
			rc.pNl(elseBlock);

			rc.getPrinter().indent();
			visitStatements(elseBlock.getStatements(), rc);
			rc.getPrinter().unindent();

			rc.p("}");
			rc.pNl();
		}

		rc.pNl();
	}

	@Override
	public Class<IfStatementContext> from() {
		return IfStatementContext.class;
	}

	protected void visitStatements(final List<Statement> statements, final RuleContext rc) {
		for (final Statement statement : statements) {
			rc.visit(statement.getCtx());
		}
	}
}
