package io.proleap.cobol.transform.java.rules.lang.data.file;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.FileDescriptionEntryContext;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.transform.java.identifier.variable.JavaFileDescriptionEntryIdentifierService;
import io.proleap.cobol.transform.java.type.JavaTypeService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class FileDescriptionEntryRule extends CobolTransformRule<FileDescriptionEntryContext, FileDescriptionEntry> {

	@Inject
	private JavaFileDescriptionEntryIdentifierService javaFileDescriptionEntryIdentifierService;

	@Inject
	private JavaTypeService javaTypeService;

	@Override
	public void apply(final FileDescriptionEntryContext ctx, final FileDescriptionEntry fileDescriptionEntry,
			final RuleContext rc) {
		final String className = javaTypeService.mapToType(fileDescriptionEntry);

		rc.p("public class %s {", className);
		rc.pNl(fileDescriptionEntry);
		rc.getPrinter().indent();

		for (final DataDescriptionEntry dataDescriptionEntry : fileDescriptionEntry.getRootDataDescriptionEntries()) {
			rc.visit(dataDescriptionEntry.getCtx());
		}

		rc.getPrinter().unindent();
		rc.p("}");
		rc.pNl();

		printInstance(fileDescriptionEntry, className, rc);
	}

	@Override
	public Class<FileDescriptionEntryContext> from() {
		return FileDescriptionEntryContext.class;
	}

	private void printInstance(final FileDescriptionEntry fileDescriptionEntry, final String className,
			final RuleContext rc) {
		final String fileDescriptionEntryIdentifier = javaFileDescriptionEntryIdentifierService
				.mapToIdentifier(fileDescriptionEntry);

		rc.p("public %s %s = new %s();", className, fileDescriptionEntryIdentifier, className);
		rc.pNl();
	}
}
