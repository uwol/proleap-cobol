package io.proleap.cobol.transform.java.rules.lang.procedure.read;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ReadStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.read.Into;
import io.proleap.cobol.asg.metamodel.procedure.read.ReadStatement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ReadStatementRule extends CobolTransformRule<ReadStatementContext, ReadStatement> {

	@Override
	public void apply(final ReadStatementContext ctx, final ReadStatement readStatement, final RuleContext rc) {
		final Into into = readStatement.getInto();

		if (into == null) {
			read(readStatement, rc);
		} else {
			readInto(readStatement, rc, into);
		}
	}

	@Override
	public Class<ReadStatementContext> from() {
		return ReadStatementContext.class;
	}

	protected void read(final ReadStatement readStatement, final RuleContext rc) {
		rc.p("fileControlService.read(");
		rc.visit(readStatement.getFileCall().getCtx());
		rc.p(");");
		rc.pNl(readStatement);
	}

	protected void readInto(final ReadStatement readStatement, final RuleContext rc, final Into into) {
		rc.p("fileControlService.read(");
		rc.visit(readStatement.getFileCall().getCtx());
		rc.p(", ");
		rc.visit(into.getIntoCall().getCtx());
		rc.p(");");
		rc.pNl(readStatement);
	}
}
