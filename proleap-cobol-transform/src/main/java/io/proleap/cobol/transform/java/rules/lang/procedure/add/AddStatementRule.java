package io.proleap.cobol.transform.java.rules.lang.procedure.add;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.AddStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.add.AddStatement;
import io.proleap.cobol.asg.metamodel.procedure.add.AddStatement.AddType;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class AddStatementRule extends CobolTransformRule<AddStatementContext, AddStatement> {

	@Override
	public void apply(final AddStatementContext ctx, final AddStatement addStatement, final RuleContext rc) {
		final AddType type = addStatement.getAddType();

		switch (type) {
		case CORRESPONDING:
			rc.visit(addStatement.getAddCorrespondingStatement().getCtx());
			break;
		case TO_GIVING:
			rc.visit(addStatement.getAddToGivingStatement().getCtx());
			break;
		case TO:
			rc.visit(addStatement.getAddToStatement().getCtx());
			break;
		default:
			break;
		}
	}

	@Override
	public Class<AddStatementContext> from() {
		return AddStatementContext.class;
	}
}
