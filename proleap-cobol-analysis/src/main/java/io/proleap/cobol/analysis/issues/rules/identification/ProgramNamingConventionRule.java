package io.proleap.cobol.analysis.issues.rules.identification;

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
import io.proleap.cobol.asg.metamodel.identification.ProgramIdParagraph;

@Singleton
public class ProgramNamingConventionRule extends IssueRule {

	protected final String DESCRIPTION = "Program names should comply with a naming convention at %s";

	@Inject
	private ProgramNamingConventionFeatureGenerator featureGenerator;

	@Override
	public List<IssueDto> apply(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry) {
		final Stream<ProgramIdParagraph> programIdParagraphs = featureGenerator.getAll(compilationUnit);
		final List<ParserRuleContext> ctxs = programIdParagraphs.map(asgElement -> {
			return asgElement.getCtx();
		}).collect(Collectors.toList());

		return entriesFromIds(ctxs, compilationUnit, idRegistry, DESCRIPTION, SeverityEnum.MINOR);
	}
}
