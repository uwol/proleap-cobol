package io.proleap.cobol.analysis.issues.rules.procedure.nextsentence;

import java.util.List;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.Else;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.IfStatement;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.Then;
import io.proleap.cobol.asg.metamodel.procedure.search.SearchStatement;
import io.proleap.cobol.asg.metamodel.procedure.search.WhenPhrase;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class NextSentenceShouldNotBeUsedFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isRelevantIfStatement(final IfStatement ifStatement) {
		final boolean result;
		final Then then = ifStatement.getThen();

		if (then == null) {
			result = false;
		} else if (!then.isNextSentence()) {
			final Else ifelse = ifStatement.getElse();

			if (ifelse == null) {
				result = false;
			} else {
				result = ifelse.isNextSentence();
			}
		} else {
			result = true;
		}

		return result;
	}

	protected boolean isRelevantSearchStatement(final SearchStatement searchStatement) {
		boolean result;

		final List<WhenPhrase> whenPhrases = searchStatement.getWhenPhrases();

		if (whenPhrases == null) {
			result = false;
		} else if (whenPhrases.size() < 1) {
			result = false;
		} else {
			result = false;

			for (final WhenPhrase when : whenPhrases) {
				if (WhenPhrase.WhenType.NEXT_SENTENCE.equals(when.getWhenType())) {
					result = true;
				}
			}
		}

		return result;
	}

	protected boolean isRelevantStatement(final Statement statement) {
		boolean isRelevantStatement = false;

		if (StatementTypeEnum.IF.equals(statement.getStatementType())) {
			final IfStatement ifStatement = (IfStatement) statement;
			isRelevantStatement = isRelevantIfStatement(ifStatement);
		} else if (StatementTypeEnum.SEARCH.equals(statement.getStatementType())) {
			final SearchStatement searchStatement = (SearchStatement) statement;
			isRelevantStatement = isRelevantSearchStatement(searchStatement);
		}

		return isRelevantStatement;
	}
}
