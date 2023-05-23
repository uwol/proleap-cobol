package io.proleap.cobol.analysis.issues.rules.data.file;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.CobolBaseVisitor;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.FileControlEntryCall;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class DeclaredFilesShouldBeUsedFeatureGenerator extends FeatureGenerator<FileControlEntry> {

	@Override
	public Stream<FileControlEntry> getAll(final CompilationUnit compilationUnit) {
		final List<FileControlEntry> calledFileControlEntrys = getCalledDataDescriptonEntries(compilationUnit);
		final List<FileControlEntry> fileControllEntries = getFileDescriptionEntries(compilationUnit);

		fileControllEntries.removeIf(fileControlEntry -> calledFileControlEntrys.contains(fileControlEntry));

		return fileControllEntries.stream();
	}

	protected List<FileControlEntry> getCalledDataDescriptonEntries(final CompilationUnit compilationUnit) {
		final List<FileControlEntry> calledDataDescriptionEntries = new ArrayList<FileControlEntry>();
		final List<ProcedureDivision> procedureDivisions = CobolStreamUtils.procedureDivisions(compilationUnit)
				.collect(Collectors.toList());

		final CobolBaseVisitor<Boolean> fileNameVisitor = new CobolBaseVisitor<Boolean>() {

			@Override
			public Boolean visitFileName(final CobolParser.FileNameContext ctx) {
				final Call call = (Call) compilationUnit.getProgram().getASGElementRegistry().getASGElement(ctx);

				if (CallType.FILE_CONTROL_ENTRY_CALL.equals(call.getCallType())) {
					final FileControlEntryCall fileControlEntryCall = (FileControlEntryCall) call.unwrap();
					final FileControlEntry fileControlEntry = fileControlEntryCall.getFileControlEntry();

					if (calledDataDescriptionEntries.contains(fileControlEntry)) {
						return false;
					}

					calledDataDescriptionEntries.add(fileControlEntry);
				}

				return true;
			}
		};

		procedureDivisions.forEach(procedureDivision -> fileNameVisitor.visit(procedureDivision.getCtx()));

		return calledDataDescriptionEntries;
	}

	protected List<FileControlEntry> getFileDescriptionEntries(final CompilationUnit compilationUnit) {
		final List<FileControlEntry> fileControlEntries = new ArrayList<FileControlEntry>();

		final CobolBaseVisitor<Boolean> fileNameVisitor = new CobolBaseVisitor<Boolean>() {

			@Override
			public Boolean visitFileName(final CobolParser.FileNameContext ctx) {
				final Call call = (Call) compilationUnit.getProgram().getASGElementRegistry().getASGElement(ctx);

				if (CallType.FILE_CONTROL_ENTRY_CALL.equals(call.getCallType())) {
					final FileControlEntryCall fileControlEntryCall = (FileControlEntryCall) call.unwrap();
					final FileControlEntry fileControlEntry = fileControlEntryCall.getFileControlEntry();

					if (fileControlEntries.contains(fileControlEntry)) {
						return false;
					}

					fileControlEntries.add(fileControlEntry);
				}

				return true;
			}
		};

		fileNameVisitor.visit(compilationUnit.getCtx());

		return fileControlEntries;
	}
}
