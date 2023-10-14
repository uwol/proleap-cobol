package io.proleap.cobol.transform.java.rules.lang.call;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.TableCallContext;
import io.proleap.cobol.asg.metamodel.call.TableCall;
import io.proleap.cobol.transform.java.expression.JavaExpressionService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class TableCallRule extends CobolTransformRule<TableCallContext, TableCall> {

	@Inject
	private JavaExpressionService javaExpressionService;

	@Override
	public void apply(final TableCallContext ctx, final TableCall call, final RuleContext rc) {
		rc.p(javaExpressionService.mapToCall(call));
	}

	@Override
	public Class<TableCallContext> from() {
		return TableCallContext.class;
	}
}
