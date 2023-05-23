package io.proleap.cobol.analysis.issues.rules;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.antlr.v4.runtime.ParserRuleContext;

import io.proleap.cobol.analysis.issues.dto.IssueDto;
import io.proleap.cobol.analysis.issues.dto.SeverityEnum;
import io.proleap.cobol.analysis.registry.CobolIdRegistry;
import io.proleap.cobol.analysis.util.NamingUtils;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.registry.ASGElementRegistry;

public abstract class IssueRule {

	public abstract List<IssueDto> apply(CompilationUnit compilationUnit, CobolIdRegistry idRegistry);

	protected String context(final CompilationUnit compilationUnit) {
		return compilationUnit.getName();
	}

	protected List<IssueDto> entries(final IssueDto entry) {
		return Arrays.asList(entry);
	}

	protected List<IssueDto> entriesFromIds(final List<ParserRuleContext> ctxs, final CompilationUnit compilationUnit,
			final CobolIdRegistry idRegistry, final String message, final SeverityEnum severity) {
		final List<IssueDto> result;

		if (ctxs.isEmpty()) {
			result = null;
		} else {
			result = new ArrayList<IssueDto>();

			for (final ParserRuleContext ctx : ctxs) {
				final Program program = compilationUnit.getProgram();
				final ASGElementRegistry asgElementRegistry = program.getASGElementRegistry();

				final String path = idRegistry.assureAbsoluteId(ctx, asgElementRegistry);
				final String name = NamingUtils.determineFullQualifiedName(ctx, compilationUnit);

				final String text = String.format(message, name);
				final IssueDto entry = entry(context(compilationUnit), text, severity, path);
				result.add(entry);
			}
		}

		return result;
	}

	protected IssueDto entry(final String context, final String description, final SeverityEnum severity,
			final String href) {
		return new IssueDto(context, description, severity, href);
	}

	protected String id(final ParserRuleContext ctx, final CompilationUnit compilationUnit,
			final CobolIdRegistry idRegistry) {
		final ASGElementRegistry asgElementRegistry = compilationUnit.getProgram().getASGElementRegistry();
		return idRegistry.assureAbsoluteId(ctx, asgElementRegistry);
	}

	@Override
	public String toString() {
		return getClass().getSimpleName();
	}
}
