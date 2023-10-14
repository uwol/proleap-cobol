package io.proleap.cobol.transform.java.rules.lang;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.LiteralContext;
import io.proleap.cobol.asg.metamodel.Literal;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class CobolLiteralRule extends CobolTransformRule<LiteralContext, Literal> {

	@Override
	public void apply(final LiteralContext ctx, final Literal literal, final RuleContext rc) {

		switch (literal.getLiteralType()) {
		case BOOLEAN:
			printBoolean(literal, rc);
			break;
		case NON_NUMERIC:
			printNonNumeric(literal, rc);
			break;
		case NUMERIC:
			printNumeric(literal, rc);
			break;
		case FIGURATIVE_CONSTANT:
			printFigurativeConstant(literal, rc);
			break;
		case CICS_DFH_RESP:
			break;
		case CICS_DFH_VALUE:
			break;
		default:
			break;
		}
	}

	protected String escapeBackslash(final String str) {
		final String result = str.replace("\\", "\\\\");
		return result;
	}

	protected String escapeQuote(final String str) {
		final String result = str.replaceAll("\"\"", "\\\\\"");
		return result;
	}

	@Override
	public Class<LiteralContext> from() {
		return LiteralContext.class;
	}

	protected void printBoolean(final Literal literal, final RuleContext rc) {
		rc.visit(literal.getBooleanLiteral().getCtx());
	}

	protected void printFigurativeConstant(final Literal literal, final RuleContext rc) {
		rc.visit(literal.getFigurativeConstant().getCtx());
	}

	protected void printNonNumeric(final Literal literal, final RuleContext rc) {
		final String nonNumericLiteral = literal.getNonNumericLiteral();
		final String nonNumericLiteralEscaped = escapeQuote(escapeBackslash(nonNumericLiteral));
		rc.p("\"%s\"", nonNumericLiteralEscaped);
	}

	protected void printNumeric(final Literal literal, final RuleContext rc) {
		rc.visit(literal.getNumericLiteral().getCtx());
	}
}
