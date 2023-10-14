package io.proleap.cobol.transform.java.rules.lang.procedure.compute;

import java.math.BigDecimal;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ComputeStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.compute.ComputeStatement;
import io.proleap.cobol.asg.metamodel.procedure.compute.Store;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.transform.java.util.JavaLiteralUtils;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ComputeStatementRule extends CobolTransformRule<ComputeStatementContext, ComputeStatement> {

	@Inject
	private CobolValueService valueService;

	@Inject
	private CobolValueStmtService valueStmtService;

	@Override
	public void apply(final ComputeStatementContext ctx, final ComputeStatement computeStatement,
			final RuleContext rc) {
		final ArithmeticValueStmt arithmeticExpression = computeStatement.getArithmeticExpression();

		final CobolValue value = valueStmtService.getValue(arithmeticExpression, null);
		final BigDecimal decimalValue = valueService.getDecimal(value);

		for (final Store store : computeStatement.getStores()) {
			rc.visit(store.getCtx());
			rc.p(" = ");

			if (decimalValue != null) {
				rc.p(JavaLiteralUtils.mapToLiteral(decimalValue));
			} else {
				rc.visit(arithmeticExpression.getCtx());
			}

			rc.p(";");
			rc.pNl(store);
		}
	}

	@Override
	public Class<ComputeStatementContext> from() {
		return ComputeStatementContext.class;
	}
}
