package io.proleap.cobol.transform.java.rules.lang.procedure;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ProcedureSectionContext;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.transform.java.identifier.method.JavaMethodIdentifierService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class SectionRule extends CobolTransformRule<ProcedureSectionContext, Section> {

	@Inject
	private JavaMethodIdentifierService javaMethodIdentifierService;

	@Override
	public void apply(final ProcedureSectionContext ctx, final Section section, final RuleContext rc) {
		rc.p("public void %s() throws Exception {", javaMethodIdentifierService.mapToIdentifier(section));
		rc.pNl(section);
		rc.getPrinter().indent();

		for (final Statement statement : section.getStatements()) {
			rc.visit(statement.getCtx());
		}

		for (final Paragraph paragraph : section.getParagraphs()) {
			rc.p("%s();", javaMethodIdentifierService.mapToIdentifier(paragraph));
			rc.pNl();
		}

		rc.getPrinter().unindent();
		rc.p("}");
		rc.pNl();

		rc.pNl();

		for (final Paragraph paragraph : section.getParagraphs()) {
			rc.visit(paragraph.getCtx());
		}
	}

	@Override
	public Class<ProcedureSectionContext> from() {
		return ProcedureSectionContext.class;
	}
}
