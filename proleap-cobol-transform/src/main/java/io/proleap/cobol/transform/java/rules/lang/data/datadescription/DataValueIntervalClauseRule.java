package io.proleap.cobol.transform.java.rules.lang.data.datadescription;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.DataValueIntervalContext;
import io.proleap.cobol.asg.metamodel.data.datadescription.ValueInterval;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class DataValueIntervalClauseRule extends CobolTransformRule<DataValueIntervalContext, ValueInterval> {

	@Override
	public void apply(final DataValueIntervalContext ctx, final ValueInterval valueInterval, final RuleContext rc) {
		final ValueStmt fromValueStmt = valueInterval.getFromValueStmt();
		rc.visit(fromValueStmt.getCtx());
	}

	@Override
	public Class<DataValueIntervalContext> from() {
		return DataValueIntervalContext.class;
	}
}
