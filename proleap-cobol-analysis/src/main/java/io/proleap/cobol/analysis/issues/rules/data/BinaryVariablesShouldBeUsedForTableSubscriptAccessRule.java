package io.proleap.cobol.analysis.issues.rules.data;

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
import io.proleap.cobol.asg.metamodel.call.TableCall;

@Singleton
public class BinaryVariablesShouldBeUsedForTableSubscriptAccessRule extends IssueRule {

	protected final String DESCRIPTION = "Using a non-BINARY/COMP variable to access table's elements at %s is inefficient.";

	@Inject
	private BinaryVariablesShouldBeUsedForTableSubscriptAccessFeatureGenerator featureGenerator;

	@Override
	public List<IssueDto> apply(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry) {
		final Stream<TableCall> entries = featureGenerator.getAll(compilationUnit);
		final List<ParserRuleContext> ctxs = entries.map(dataDescriptionEntry -> {
			return dataDescriptionEntry.getCtx();
		}).collect(Collectors.toList());

		return entriesFromIds(ctxs, compilationUnit, idRegistry, DESCRIPTION, SeverityEnum.MINOR);
	}
}
