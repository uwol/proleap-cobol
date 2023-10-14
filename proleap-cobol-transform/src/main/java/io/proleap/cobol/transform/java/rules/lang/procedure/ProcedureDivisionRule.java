package io.proleap.cobol.transform.java.rules.lang.procedure;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ProcedureDivisionContext;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ProcedureDivisionRule extends CobolTransformRule<ProcedureDivisionContext, ProcedureDivision> {

	@Override
	public void apply(final ProcedureDivisionContext ctx, final ProcedureDivision procedureDivision,
			final RuleContext rc) {
		final boolean hasStatements = !procedureDivision.getStatements().isEmpty();

		if (hasStatements) {
			rc.p("public void procedureDivision() throws Exception{");
			rc.pNl(procedureDivision);
			rc.getPrinter().indent();

			for (final Statement statement : procedureDivision.getStatements()) {
				rc.visit(statement.getCtx());
			}

			rc.getPrinter().unindent();
			rc.p("}");
			rc.pNl();
		}

		for (final Section section : procedureDivision.getSections()) {
			rc.visit(section.getCtx());
		}

		for (final Paragraph paragraph : procedureDivision.getRootParagraphs()) {
			rc.visit(paragraph.getCtx());
		}
	}

	@Override
	public Class<ProcedureDivisionContext> from() {
		return ProcedureDivisionContext.class;
	}
}
