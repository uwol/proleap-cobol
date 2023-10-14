package io.proleap.cobol.transform.java.rules.lang.identification;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.IdentificationDivisionContext;
import io.proleap.cobol.asg.metamodel.identification.IdentificationDivision;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class IdentificationDivisionRule extends CobolTransformRule<IdentificationDivisionContext, IdentificationDivision> {

	@Override
	public void apply(final IdentificationDivisionContext ctx, final IdentificationDivision identificationDivision,
			final RuleContext rc) {
		rc.p("/**");
		rc.pNl(identificationDivision);

		if (ctx.programIdParagraph() != null) {
			rc.visit(ctx.programIdParagraph());
		}

		if (identificationDivision.getAuthorParagraph() != null) {
			rc.visit(identificationDivision.getAuthorParagraph().getCtx());
		}

		if (identificationDivision.getInstallationParagraph() != null) {
			rc.visit(identificationDivision.getInstallationParagraph().getCtx());
		}

		if (identificationDivision.getDateWrittenParagraph() != null) {
			rc.visit(identificationDivision.getDateWrittenParagraph().getCtx());
		}

		if (identificationDivision.getDateCompiledParagraph() != null) {
			rc.visit(identificationDivision.getDateCompiledParagraph().getCtx());
		}

		if (identificationDivision.getSecurityParagraph() != null) {
			rc.visit(identificationDivision.getSecurityParagraph().getCtx());
		}

		if (identificationDivision.getRemarksParagraph() != null) {
			rc.visit(identificationDivision.getRemarksParagraph().getCtx());
		}

		rc.p("*/");
		rc.pNl();
	}

	@Override
	public Class<IdentificationDivisionContext> from() {
		return IdentificationDivisionContext.class;
	}
}
