package io.proleap.cobol.transform.java.rules.lang.call;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.CobolParser.DataDescNameContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class DataDescCallRule extends CobolTransformRule<DataDescNameContext, Call> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final DataDescNameContext ctx, final Call call, final RuleContext rc) {
		rc.p(javaExpressionService.mapToCall(call));
	}

	@Override
	public Class<DataDescNameContext> from() {
		return DataDescNameContext.class;
	}
}
