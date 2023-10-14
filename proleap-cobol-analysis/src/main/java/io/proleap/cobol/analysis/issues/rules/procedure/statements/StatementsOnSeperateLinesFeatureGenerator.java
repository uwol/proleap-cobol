package io.proleap.cobol.analysis.issues.rules.procedure.statements;

import java.util.HashSet;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import org.antlr.v4.runtime.ParserRuleContext;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.ProgramUnitElement;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class StatementsOnSeperateLinesFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		final HashSet<Integer> statementLines = new HashSet<Integer>();
		statementLines.clear();
		return CobolStreamUtils.statementsFlat(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement, statementLines);
		});
	}

	protected Integer getSourceLineNumber(final ProgramUnitElement element) {
		final ParserRuleContext ctx = element.getCtx();
		final int result = ctx.start.getLine() - 1;
		return result;
	}

	protected boolean isRelevantStatement(final Statement statement, final HashSet<Integer> statementLines) {
		final Integer statementLine = getSourceLineNumber(statement);

		if (statementLines.contains(statementLine)) {
			return true;
		} else {
			statementLines.add(statementLine);
			return false;
		}
	}
}
