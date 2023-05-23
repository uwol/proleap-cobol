package io.proleap.cobol.transform.java.rules.lang.call;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.CobolParser.RecordNameContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class RecordCallRule extends CobolTransformRule<RecordNameContext, Call> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final RecordNameContext ctx, final Call call, final RuleContext rc) {
		rc.p(javaExpressionService.mapToCall(call));
	}

	@Override
	public Class<RecordNameContext> from() {
		return RecordNameContext.class;
	}
}
