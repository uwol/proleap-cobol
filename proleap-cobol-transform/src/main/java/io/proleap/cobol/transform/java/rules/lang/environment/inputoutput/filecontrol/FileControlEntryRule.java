package io.proleap.cobol.transform.java.rules.lang.environment.inputoutput.filecontrol;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.FileControlEntryContext;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.AssignClause;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.SelectClause;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.transform.java.identifier.JavaIdentifierService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class FileControlEntryRule extends CobolTransformRule<FileControlEntryContext, FileControlEntry> {

	@Inject
	private JavaIdentifierService javaIdentifierService;

	@Inject
	private CobolValueStmtService valueStmtService;

	@Override
	public void apply(final FileControlEntryContext ctx, final FileControlEntry fileControlEntry,
			final RuleContext rc) {
		final SelectClause selectClause = fileControlEntry.getSelectClause();
		final AssignClause assignClause = fileControlEntry.getAssignClause();

		rc.p("FileControlEntry %s = new FileControlEntry(",
				javaIdentifierService.mapToIdentifier(selectClause.getName()));

		if (assignClause != null) {
			final ValueStmt toValueStmt = assignClause.getToValueStmt();

			if (toValueStmt != null) {
				final Object value = valueStmtService.getValue(toValueStmt, null);

				if (value != null) {
					rc.p("\"%s\"", value);
				} else {
					rc.p("\"");
					rc.visit(assignClause.getToValueStmt().getCtx());
					rc.p("\"");
				}
			}
		}

		rc.p(");");
		rc.pNl(selectClause);
	}

	@Override
	public Class<FileControlEntryContext> from() {
		return FileControlEntryContext.class;
	}
}
