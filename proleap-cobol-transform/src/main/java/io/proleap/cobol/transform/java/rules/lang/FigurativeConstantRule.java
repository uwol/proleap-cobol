package io.proleap.cobol.transform.java.rules.lang;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.FigurativeConstantContext;
import io.proleap.cobol.asg.metamodel.FigurativeConstant;
import io.proleap.cobol.asg.metamodel.FigurativeConstant.FigurativeConstantType;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class FigurativeConstantRule extends CobolTransformRule<FigurativeConstantContext, FigurativeConstant> {

	@Override
	public void apply(final FigurativeConstantContext ctx, final FigurativeConstant figurativeConstant,
			final RuleContext rc) {
		final FigurativeConstantType type = figurativeConstant.getFigurativeConstantType();

		switch (type) {
		case ALL:
			break;
		case HIGH_VALUE:
		case HIGH_VALUES:
			break;
		case LOW_VALUE:
		case LOW_VALUES:
			break;
		case NULL:
		case NULLS:
			break;
		case QUOTE:
		case QUOTES:
			break;
		case SPACE:
		case SPACES:
			rc.p("\" \"");
			break;
		case ZERO:
		case ZEROES:
		case ZEROS:
			rc.p("BigDecimal.ZERO");
			break;
		default:
			break;
		}
	}

	@Override
	public Class<FigurativeConstantContext> from() {
		return FigurativeConstantContext.class;
	}
}
