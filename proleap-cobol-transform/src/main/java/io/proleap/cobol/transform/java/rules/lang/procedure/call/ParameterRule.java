package io.proleap.cobol.transform.java.rules.lang.procedure.call;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.CallUsingParameterContext;
import io.proleap.cobol.asg.metamodel.procedure.call.UsingParameter;
import io.proleap.cobol.asg.metamodel.procedure.call.UsingParameter.ParameterType;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ParameterRule extends CobolTransformRule<CallUsingParameterContext, UsingParameter> {

	@Override
	public void apply(final CallUsingParameterContext ctx, final UsingParameter parameter, final RuleContext rc) {
		final ParameterType type = parameter.getParameterType();

		switch (type) {
		case REFERENCE:
			rc.visit(parameter.getByReferencePhrase().getCtx());
			break;
		case VALUE:
			rc.visit(parameter.getByValuePhrase().getCtx());
			break;
		case CONTENT:
			rc.visit(parameter.getByContentPhrase().getCtx());
			break;
		default:
			break;
		}
	}

	@Override
	public Class<CallUsingParameterContext> from() {
		return CallUsingParameterContext.class;
	}
}
