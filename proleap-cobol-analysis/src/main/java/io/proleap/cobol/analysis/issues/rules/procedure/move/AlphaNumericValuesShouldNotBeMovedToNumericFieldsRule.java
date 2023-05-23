package io.proleap.cobol.analysis.issues.rules.procedure.move;

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
import io.proleap.cobol.asg.metamodel.procedure.move.MoveStatement;

@Singleton
public class AlphaNumericValuesShouldNotBeMovedToNumericFieldsRule extends IssueRule {

	protected final String DESCRIPTION = "Alphanumeric values should not be moved to numeric fields at %s";

	@Inject
	private AlphaNumericValuesShouldNotBeMovedToNumericFieldsFeatureGenerator featureGenerator;

	@Override
	public List<IssueDto> apply(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry) {
		final Stream<MoveStatement> statements = featureGenerator.getAll(compilationUnit);
		final List<ParserRuleContext> ctxs = statements.map(dataDescriptionEntry -> {
			return dataDescriptionEntry.getCtx();
		}).collect(Collectors.toList());

		return entriesFromIds(ctxs, compilationUnit, idRegistry, DESCRIPTION, SeverityEnum.MINOR);
	}
}
