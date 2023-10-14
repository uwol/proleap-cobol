package io.proleap.cobol.analysis.issues.rules.data.file;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.environment.EnvironmentDivision;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.InputOutputSection;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlParagraph;
import io.proleap.cobol.asg.resolver.NameResolver;
import io.proleap.cobol.asg.resolver.impl.NameResolverImpl;

@Singleton
public class LogicalFileNamingConventionFeatureGenerator extends FeatureGenerator<ASGElement> {

	private static final String NAMING_CONVENTION = "FILE-.*";

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
							final NameResolver nameResolver = new NameResolverImpl();
							final String determineName = nameResolver.determineName(fileControlEntry.getCtx());

							if (determineName != null) {
								if (!isFollowingANamingConvention(determineName)) {
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

	protected boolean isFollowingANamingConvention(final String name) {
		return name.matches(NAMING_CONVENTION);
	}
}
