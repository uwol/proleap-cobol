package io.proleap.cobol.analysis.issues.rules.procedure.paragraph;

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
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;

@Singleton
public class ParagraphNamingConventionRule extends IssueRule {

	protected final String DESCRIPTION = "Paragraphs should follow a naming convention at %s";

	@Inject
	private ParagraphNamingConventionFeatureGenerator featureGenerator;

	@Override
	public List<IssueDto> apply(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry) {
		final Stream<Paragraph> paragraphs = featureGenerator.getAll(compilationUnit);
		final List<ParserRuleContext> ctxs = paragraphs.map(paragraph -> {
			return paragraph.getCtx();
		}).collect(Collectors.toList());

		return entriesFromIds(ctxs, compilationUnit, idRegistry, DESCRIPTION, SeverityEnum.MINOR);
	}
}
