package io.proleap.cobol.transform.java.rules.lang.call;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.CobolParser.FileNameContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class FileCallRule extends CobolTransformRule<FileNameContext, Call> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final FileNameContext ctx, final Call call, final RuleContext rc) {
		rc.p(javaExpressionService.mapToCall(call));
	}

	@Override
	public Class<FileNameContext> from() {
		return FileNameContext.class;
	}
}
