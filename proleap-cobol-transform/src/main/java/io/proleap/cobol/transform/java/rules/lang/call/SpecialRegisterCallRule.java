package io.proleap.cobol.transform.java.rules.lang.call;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.SpecialRegisterContext;
import io.proleap.cobol.asg.metamodel.call.SpecialRegisterCall;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class SpecialRegisterCallRule extends CobolTransformRule<SpecialRegisterContext, SpecialRegisterCall> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final SpecialRegisterContext ctx, final SpecialRegisterCall call, final RuleContext rc) {
		rc.p(javaExpressionService.mapToCall(call));
	}

	@Override
	public Class<SpecialRegisterContext> from() {
		return SpecialRegisterContext.class;
	}
}
