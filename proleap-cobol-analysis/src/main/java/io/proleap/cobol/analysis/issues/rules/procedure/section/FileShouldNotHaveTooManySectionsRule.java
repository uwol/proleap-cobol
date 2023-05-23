package io.proleap.cobol.analysis.issues.rules.procedure.section;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.inject.Singleton;

import org.antlr.v4.runtime.ParserRuleContext;

import io.proleap.cobol.analysis.issues.dto.IssueDto;
import io.proleap.cobol.analysis.issues.dto.SeverityEnum;
import io.proleap.cobol.analysis.issues.rules.IssueRule;
import io.proleap.cobol.analysis.registry.CobolIdRegistry;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.ProgramUnit;

@Singleton
public class FileShouldNotHaveTooManySectionsRule extends IssueRule {

	protected final String DESCRIPTION = "The file should not contain too many sections.";

	@Inject
	private FileShouldNotHaveTooManySectionsFeatureGenerator featureGenerator;

	@Override
	public List<IssueDto> apply(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry) {
		final Stream<ProgramUnit> sections = featureGenerator.getAll(compilationUnit);
		final List<ParserRuleContext> ctxs = sections.map(section -> {
			return section.getCtx();
		}).collect(Collectors.toList());

		return entriesFromIds(ctxs, compilationUnit, idRegistry, DESCRIPTION, SeverityEnum.MAJOR);
	}
}
