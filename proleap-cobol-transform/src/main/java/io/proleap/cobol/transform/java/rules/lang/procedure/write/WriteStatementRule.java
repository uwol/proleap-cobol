package io.proleap.cobol.transform.java.rules.lang.procedure.write;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.WriteStatementContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.procedure.write.WriteStatement;
import io.proleap.cobol.asg.util.ANTLRUtils;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.transform.java.identifier.variable.JavaFileControlEntryIdentifierService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class WriteStatementRule extends CobolTransformRule<WriteStatementContext, WriteStatement> {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Inject
	private JavaFileControlEntryIdentifierService javaFileControlEntryIdentifierService;

	@Override
	public void apply(final WriteStatementContext ctx, final WriteStatement writeStatement, final RuleContext rc) {
		rc.p("fileControlService.write(");

		final Call recordCall = writeStatement.getRecordCall();
		final DataDescriptionEntry dataDescriptionEntry = cobolDataDescriptionEntryService
				.getDataDescriptionEntry(recordCall);

		if (dataDescriptionEntry == null) {
			visitRecordCall(writeStatement, rc);
		} else {
			printFileDescriptionEntry(writeStatement, dataDescriptionEntry, rc);
		}

		rc.p(");");
		rc.pNl(writeStatement);
	}

	@Override
	public Class<WriteStatementContext> from() {
		return WriteStatementContext.class;
	}

	protected void printFileDescriptionEntry(final WriteStatement writeStatement,
			final DataDescriptionEntry dataDescriptionEntry, final RuleContext rc) {
		final FileDescriptionEntry fileDescriptionEntry = (FileDescriptionEntry) ANTLRUtils.findParent(
				FileDescriptionEntry.class, dataDescriptionEntry.getCtx(),
				writeStatement.getProgram().getASGElementRegistry());

		if (fileDescriptionEntry == null) {
			visitRecordCall(writeStatement, rc);
		} else {
			final FileControlEntry fileControlEntry = fileDescriptionEntry.getFileControlEntry();

			if (fileControlEntry == null) {
				visitRecordCall(writeStatement, rc);
			} else {
				final String identifier = javaFileControlEntryIdentifierService.mapToIdentifier(fileControlEntry);
				rc.p(identifier);
			}
		}
	}

	private void visitRecordCall(final WriteStatement writeStatement, final RuleContext rc) {
		rc.visit(writeStatement.getRecordCall().getCtx());
	}
}
