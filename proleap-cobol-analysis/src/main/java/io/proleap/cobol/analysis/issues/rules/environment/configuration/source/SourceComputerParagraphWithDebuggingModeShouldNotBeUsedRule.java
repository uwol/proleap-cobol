package io.proleap.cobol.analysis.issues.rules.environment.configuration.source;

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
import io.proleap.cobol.asg.metamodel.environment.configuration.source.SourceComputerParagraph;

@Singleton
public class SourceComputerParagraphWithDebuggingModeShouldNotBeUsedRule extends IssueRule {

	protected final String DESCRIPTION = "WITH DEBUGGING MODE should not be used at %s";

	@Inject
	private SourceComputerParagraphWithDebuggingModeShouldNotBeUsedFeatureGenerator featureGenerator;

	@Override
	public List<IssueDto> apply(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry) {
		final Stream<SourceComputerParagraph> statements = featureGenerator.getAll(compilationUnit);
		final List<ParserRuleContext> ctxs = statements.map(dataDescriptionEntry -> {
			return dataDescriptionEntry.getCtx();
		}).collect(Collectors.toList());

		return entriesFromIds(ctxs, compilationUnit, idRegistry, DESCRIPTION, SeverityEnum.MINOR);
	}
}
