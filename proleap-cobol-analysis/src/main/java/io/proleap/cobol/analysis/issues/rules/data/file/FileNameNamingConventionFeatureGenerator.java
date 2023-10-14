package io.proleap.cobol.analysis.issues.rules.data.file;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.environment.EnvironmentDivision;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.InputOutputSection;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.AssignClause;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlParagraph;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;

@Singleton
public class FileNameNamingConventionFeatureGenerator extends FeatureGenerator<ASGElement> {

	private static final String NAMING_CONVENTION = "[a-zA-Z0-9\\x25\\x26\\x2B\\x2C\\x2D\\x2E\\x3D\\x5F\\x3A\\s]+";

	@Inject
	private CobolValueService valueService;

	@Inject
	private CobolValueStmtService valueStmtService;

	@Override
	public Stream<ASGElement> getAll(final CompilationUnit compilationUnit) {
		return getAllFiles(compilationUnit);
	}

	protected Stream<ASGElement> getAllFiles(final CompilationUnit compilationUnit) {
		final ArrayList<ASGElement> result = new ArrayList<ASGElement>();
		final ProgramUnit programUnit = compilationUnit.getProgramUnit();

		if (programUnit != null) {
			final EnvironmentDivision environmentDivision = programUnit.getEnvironmentDivision();

			if (environmentDivision != null) {
				final InputOutputSection inputOutputSection = environmentDivision.getInputOutputSection();

				if (inputOutputSection != null) {
					final FileControlParagraph fileControlParagraph = inputOutputSection.getFileControlParagraph();

					if (fileControlParagraph != null) {
						final List<FileControlEntry> fileControlEntries = fileControlParagraph.getFileControlEntries();

						for (final FileControlEntry fileControlEntry : fileControlEntries) {
							final AssignClause assignClause = fileControlEntry.getAssignClause();
							final ValueStmt valueStmt = assignClause.getToValueStmt();
							final CobolValue value = valueStmtService.getValue(valueStmt, null);
							final String castString = valueService.getString(value);

							if (castString != null) {
								if (!isFollowingNamingConvention(castString)) {
									result.add(fileControlEntry);
								}
							}
						}
					}
				}
			}
		}

		return result.stream();
	}

	protected boolean isFollowingNamingConvention(final String name) {
		return name.matches(NAMING_CONVENTION);
	}
}
