package io.proleap.cobol.analysis.issues.rules.procedure.evaluate;

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
import io.proleap.cobol.asg.metamodel.procedure.Statement;

@Singleton
public class EvaluateShouldEndWithWhenOtherRule extends IssueRule {

	protected final String DESCRIPTION = "Evaluate structures should end with 'WHEN OTHERS' clause at %s.";

	@Inject
	private EvaluateShouldEndWithWhenOtherFeatureGenerator featureGenerator;

	@Override
	public List<IssueDto> apply(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry) {
		final Stream<Statement> statements = featureGenerator.getAll(compilationUnit);
		final List<ParserRuleContext> ctxs = statements.map(dataDescriptionEntry -> {
			return dataDescriptionEntry.getCtx();
		}).collect(Collectors.toList());

		return entriesFromIds(ctxs, compilationUnit, idRegistry, DESCRIPTION, SeverityEnum.MINOR);
	}
}
