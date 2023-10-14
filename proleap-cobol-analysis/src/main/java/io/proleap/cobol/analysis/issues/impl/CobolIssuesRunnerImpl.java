package io.proleap.cobol.analysis.issues.impl;

import java.util.List;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.proleap.cobol.analysis.issues.CobolIssuesRunner;
import io.proleap.cobol.analysis.issues.dto.IssueDto;
import io.proleap.cobol.analysis.issues.dto.IssuesDto;
import io.proleap.cobol.analysis.issues.rules.IssueRule;
import io.proleap.cobol.analysis.registry.CobolIdRegistry;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;

@Singleton
public class CobolIssuesRunnerImpl implements CobolIssuesRunner {

	private final static Logger LOG = LoggerFactory.getLogger(CobolIssuesRunnerImpl.class);

	@Inject
	private List<IssueRule> rules;

	@Override
	public IssuesDto analyzeCompilationUnit(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry) {
		return applyRules(compilationUnit, idRegistry);
	}

	@Override
	public IssuesDto analyzeProgram(final Program program, final CobolIdRegistry idRegistry) {
		final IssuesDto result = new IssuesDto();

		for (final CompilationUnit compilationUnit : program.getCompilationUnits()) {
			final IssuesDto issuesDto = analyzeCompilationUnit(compilationUnit, idRegistry);

			if (issuesDto != null) {
				result.issues.addAll(issuesDto.issues);
			}
		}

		return result;
	}

	protected IssuesDto applyRules(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry) {
		final IssuesDto result = new IssuesDto();

		for (final IssueRule rule : rules) {
			final List<IssueDto> entries = rule.apply(compilationUnit, idRegistry);

			if (entries != null) {
				result.issues.addAll(entries);
			}
		}

		return result;
	}

	@PostConstruct
	public void init() {
		LOG.info("Initialized {} COBOL issues rules as {}.", rules.size(), rules);
	}
}
