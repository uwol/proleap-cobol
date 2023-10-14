package io.proleap.cobol.analysis.issues.rules.procedure.paragraph;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import org.antlr.v4.runtime.ParserRuleContext;

import io.proleap.cobol.analysis.issues.dto.IssueDto;
import io.proleap.cobol.analysis.issues.dto.SeverityEnum;
import io.proleap.cobol.analysis.issues.rules.IssueRule;
import io.proleap.cobol.analysis.registry.CobolIdRegistry;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;

@Singleton
public class RedefinedParagraphRule extends IssueRule {

	protected final String DESCRIPTION = "Paragraph %s is redefined";

	@Inject
	private RedefinedParagraphFeatureGenerator featureGenerator;

	@Override
	public List<IssueDto> apply(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry) {
		final Stream<Paragraph> paragraphs = featureGenerator.getAll(compilationUnit);
		final List<ParserRuleContext> ctxs = paragraphs.map(paragraph -> {
			return paragraph.getCtx();
		}).collect(Collectors.toList());

		return entriesFromIds(ctxs, compilationUnit, idRegistry, DESCRIPTION, SeverityEnum.CRITICAL);
	}
}
