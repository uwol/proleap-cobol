package io.proleap.cobol.transform.java.rules.lang.procedure.close;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.CloseStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.close.CloseFile;
import io.proleap.cobol.asg.metamodel.procedure.close.CloseStatement;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class CloseStatementRule extends CobolTransformRule<CloseStatementContext, CloseStatement> {

	@Override
	public void apply(final CloseStatementContext ctx, final CloseStatement closeStatement, final RuleContext rc) {
		for (final CloseFile closeFile : closeStatement.getCloseFiles()) {
			printCloseFile(closeFile, rc);
		}
	}

	@Override
	public Class<CloseStatementContext> from() {
		return CloseStatementContext.class;
	}

	protected void printCloseFile(final CloseFile closeFile, final RuleContext rc) {
		rc.p("fileControlService.close(");
		rc.visit(closeFile.getFileCall().getCtx());
		rc.p(");");
		rc.pNl(closeFile);
	}
}
