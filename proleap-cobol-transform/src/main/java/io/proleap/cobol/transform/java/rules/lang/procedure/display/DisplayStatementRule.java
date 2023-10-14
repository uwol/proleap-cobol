package io.proleap.cobol.transform.java.rules.lang.procedure.display;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.DisplayStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.display.DisplayStatement;
import io.proleap.cobol.asg.metamodel.procedure.display.Operand;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class DisplayStatementRule extends CobolTransformRule<DisplayStatementContext, DisplayStatement> {

	@Override
	public void apply(final DisplayStatementContext ctx, final DisplayStatement displayStatement,
			final RuleContext rc) {
		final int numberOfOperands = displayStatement.getOperands().size();

		if (numberOfOperands == 1) {
			rc.p("System.out.println(");

			final Operand operand = displayStatement.getOperands().get(0);
			rc.visit(operand.getCtx());

			rc.p(");");
		} else if (numberOfOperands > 1) {
			for (final Operand operand : displayStatement.getOperands()) {
				rc.p("System.out.print(");
				rc.visit(operand.getCtx());
				rc.p(");");
				rc.pNl(operand);
			}

			rc.p("System.out.println();");
		}

		rc.pNl(displayStatement);
	}

	@Override
	public Class<DisplayStatementContext> from() {
		return DisplayStatementContext.class;
	}
}
