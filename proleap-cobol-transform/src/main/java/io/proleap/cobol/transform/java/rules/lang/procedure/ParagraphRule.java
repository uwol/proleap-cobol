package io.proleap.cobol.transform.java.rules.lang.procedure;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ParagraphContext;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.transform.java.identifier.method.JavaMethodIdentifierService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ParagraphRule extends CobolTransformRule<ParagraphContext, Paragraph> {

	@Inject
	private JavaMethodIdentifierService javaMethodIdentifierService;

	@Override
	public void apply(final ParagraphContext ctx, final Paragraph paragraph, final RuleContext rc) {
		rc.p("public void %s() throws Exception {", javaMethodIdentifierService.mapToIdentifier(paragraph));
		rc.pNl(paragraph);
		rc.getPrinter().indent();

		for (final Statement statement : paragraph.getStatements()) {
			rc.visit(statement.getCtx());
		}

		rc.getPrinter().unindent();
		rc.p("}");
		rc.pNl();

		rc.pNl();
	}

	@Override
	public Class<ParagraphContext> from() {
		return ParagraphContext.class;
	}
}
