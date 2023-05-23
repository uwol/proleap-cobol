package io.proleap.cobol.analysis.issues.rules.data.file;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.IntegerLiteral;
import io.proleap.cobol.asg.metamodel.data.DataDivision;
import io.proleap.cobol.asg.metamodel.data.file.BlockContainsClause;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.file.FileSection;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class BlockContainsZeroRecordsFeatureGenerator extends FeatureGenerator<ASGElement> {

	@Override
	public Stream<ASGElement> getAll(final CompilationUnit compilationUnit) {
		return getRelevantFileDescriptionEntries(compilationUnit);
	}

	protected Stream<ASGElement> getRelevantFileDescriptionEntries(final CompilationUnit compilationUnit) {
		final ArrayList<ASGElement> result = new ArrayList<ASGElement>();
		final List<DataDivision> dataDivisions = CobolStreamUtils.dataDivisions(compilationUnit)
				.collect(Collectors.toList());

		for (final DataDivision dataDivision : dataDivisions) {
			final FileSection fileSection = dataDivision.getFileSection();

			if (fileSection != null) {
				final List<FileDescriptionEntry> fileDescriptionEntries = fileSection.getFileDescriptionEntries();

				for (final FileDescriptionEntry fileDescriptionEntry : fileDescriptionEntries) {
					final BlockContainsClause blockContainsClause = fileDescriptionEntry.getBlockContainsClause();

					if (blockContainsClause != null) {
						final IntegerLiteral from = blockContainsClause.getFrom();

						if (from != null) {
							final BigDecimal value = from.getValue();

							if (!value.equals(new BigDecimal(0))) {
								result.add(fileDescriptionEntry);
							}
						}
					}
				}
			}
		}

		return result.stream();
	}
}
