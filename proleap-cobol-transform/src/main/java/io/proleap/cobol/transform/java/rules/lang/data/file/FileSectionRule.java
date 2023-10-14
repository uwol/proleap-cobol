package io.proleap.cobol.transform.java.rules.lang.data.file;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.FileSectionContext;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.file.FileSection;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class FileSectionRule extends CobolTransformRule<FileSectionContext, FileSection> {

	@Override
	public void apply(final FileSectionContext ctx, final FileSection fileSection, final RuleContext rc) {
		for (final FileDescriptionEntry fileDescriptionEntry : fileSection.getFileDescriptionEntries()) {
			rc.visit(fileDescriptionEntry.getCtx());
		}
	}

	@Override
	public Class<FileSectionContext> from() {
		return FileSectionContext.class;
	}
}
