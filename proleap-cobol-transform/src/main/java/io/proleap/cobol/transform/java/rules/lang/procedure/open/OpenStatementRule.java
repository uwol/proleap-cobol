package io.proleap.cobol.transform.java.rules.lang.procedure.open;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.OpenStatementContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.procedure.open.ExtendPhrase;
import io.proleap.cobol.asg.metamodel.procedure.open.Input;
import io.proleap.cobol.asg.metamodel.procedure.open.InputOutputPhrase;
import io.proleap.cobol.asg.metamodel.procedure.open.InputPhrase;
import io.proleap.cobol.asg.metamodel.procedure.open.OpenStatement;
import io.proleap.cobol.asg.metamodel.procedure.open.Output;
import io.proleap.cobol.asg.metamodel.procedure.open.OutputPhrase;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class OpenStatementRule extends CobolTransformRule<OpenStatementContext, OpenStatement> {

	@Override
	public void apply(final OpenStatementContext ctx, final OpenStatement openStatement, final RuleContext rc) {
		printOpenInputs(openStatement, rc);
		printOpenInputOutputs(openStatement, rc);
		printOpenOutputs(openStatement, rc);
		printOpenExtends(openStatement, rc);
	}

	@Override
	public Class<OpenStatementContext> from() {
		return OpenStatementContext.class;
	}

	protected void printOpenExtends(final OpenStatement openStatement, final RuleContext rc) {
		for (final ExtendPhrase openExtend : openStatement.getExtendPhrases()) {
			for (final Call call : openExtend.getFileCalls()) {
				rc.p("fileControlService.openExtend(");
				rc.visit(call.getCtx());
				rc.p(");");
				rc.pNl(call);
			}
		}
	}

	protected void printOpenInputOutputs(final OpenStatement openStatement, final RuleContext rc) {
		for (final InputOutputPhrase openInputOutput : openStatement.getInputOutputPhrases()) {
			for (final Call call : openInputOutput.getFileCalls()) {
				rc.p("fileControlService.openInputOutput(");
				rc.visit(call.getCtx());
				rc.p(");");
				rc.pNl(call);
			}
		}
	}

	protected void printOpenInputs(final OpenStatement openStatement, final RuleContext rc) {
		for (final InputPhrase openInput : openStatement.getInputPhrases()) {
			for (final Input input : openInput.getInputs()) {
				rc.p("fileControlService.openInput(");
				rc.visit(input.getFileCall().getCtx());
				rc.p(");");
				rc.pNl(input);
			}
		}
	}

	protected void printOpenOutputs(final OpenStatement openStatement, final RuleContext rc) {
		for (final OutputPhrase openOutput : openStatement.getOutputPhrases()) {
			for (final Output output : openOutput.getOutputs()) {
				rc.p("fileControlService.openOutput(");
				rc.visit(output.getFileCall().getCtx());
				rc.p(");");
				rc.pNl(output);
			}
		}
	}
}
