package io.proleap.cobol.analysis.issues.rules.procedure;

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
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.CompilationUnit;

@Singleton
public class DisallowedCharectersShouldNotBeUsedRule extends IssueRule {

	protected final String DESCRIPTION = "Disallowed characters should not be used in identifiers at %s";

	@Inject
	private DisallowedCharactersShouldNotBeUsedFeatureGenerator featureGenerator;

	@Override
	public List<IssueDto> apply(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry) {
		final Stream<ASGElement> entries = featureGenerator.getAll(compilationUnit);
		final List<ParserRuleContext> ctxs = entries.map(asgElement -> {
			return asgElement.getCtx();
		}).collect(Collectors.toList());

		return entriesFromIds(ctxs, compilationUnit, idRegistry, DESCRIPTION, SeverityEnum.MINOR);
	}
}
