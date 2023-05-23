package io.proleap.cobol.analysis.issues.rules.data.file;

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
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;

@Singleton
public class DeclaredFilesShouldBeUsedRule extends IssueRule {

	protected final String DESCRIPTION = "Declared File at %s is not Used";

	@Inject
	private DeclaredFilesShouldBeUsedFeatureGenerator featureGenerator;

	@Override
	public List<IssueDto> apply(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry) {
		final Stream<FileControlEntry> entries = featureGenerator.getAll(compilationUnit);
		final List<ParserRuleContext> ctxs = entries.map(fileControlEntry -> {
			return fileControlEntry.getCtx();
		}).collect(Collectors.toList());

		return entriesFromIds(ctxs, compilationUnit, idRegistry, DESCRIPTION, SeverityEnum.MINOR);
	}
}
