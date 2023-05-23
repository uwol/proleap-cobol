package io.proleap.cobol.analysis.issues;

import io.proleap.cobol.analysis.issues.dto.IssuesDto;
import io.proleap.cobol.analysis.registry.CobolIdRegistry;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;

public interface CobolIssuesRunner {

	IssuesDto analyzeCompilationUnit(CompilationUnit compilationUnit, CobolIdRegistry idRegistry);

	IssuesDto analyzeProgram(Program program, CobolIdRegistry idRegistry);
}
