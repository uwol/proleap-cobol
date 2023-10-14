package io.proleap.cobol.transform.java.rules.lang.environment.inputoutput.filecontrol;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.FileControlParagraphContext;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlParagraph;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class FileControlParagraphRule extends CobolTransformRule<FileControlParagraphContext, FileControlParagraph> {

	@Override
	public void apply(final FileControlParagraphContext ctx, final FileControlParagraph fileControlParagraph,
			final RuleContext rc) {
		rc.p("@Inject");
		rc.pNl(fileControlParagraph);
		rc.p("FileControlService fileControlService;");
		rc.pNl();
		rc.pNl();

		rc.visitChildren(ctx);
	}

	@Override
	public Class<FileControlParagraphContext> from() {
		return FileControlParagraphContext.class;
	}
}
