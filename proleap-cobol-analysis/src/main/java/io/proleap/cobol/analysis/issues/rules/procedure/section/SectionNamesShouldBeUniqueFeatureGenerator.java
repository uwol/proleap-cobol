package io.proleap.cobol.analysis.issues.rules.procedure.section;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class SectionNamesShouldBeUniqueFeatureGenerator extends FeatureGenerator<Section> {

	@Override
	public Stream<Section> getAll(final CompilationUnit compilationUnit) {
		final List<String> nameList = new ArrayList<String>();
		final Set<Section> sectionSet = new HashSet<Section>();
		final Set<Section> result = new HashSet<Section>();

		CobolStreamUtils.sections(compilationUnit).forEach(sectionSet::add);
		CobolStreamUtils.sections(compilationUnit).map(section -> section.getName()).forEach(nameList::add);

		for (final Section section : sectionSet) {
			final int frequency = Collections.frequency(nameList, section.getName());

			if (frequency > 1) {
				result.add(section);
			}
		}

		return result.stream();
	}
}
