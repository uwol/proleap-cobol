package io.proleap.cobol.analysis.issues.rules.procedure.condition;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.Condition;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.EvaluateStatement;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.Value;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.When;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.WhenPhrase;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.util.CobolStreamUtils;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;

@Singleton
public class RelatedWhenIfStatementsSameConditionsFeatureGenerator extends FeatureGenerator<Statement> {

	@Inject
	private CobolValueService valueService;

	@Inject
	private CobolValueStmtService valueStmtService;

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isRelevantEvaluateStatement(final EvaluateStatement evalStatement) {
		final List<WhenPhrase> whenPhrases = evalStatement.getWhenPhrases();
		final boolean result;

		if (whenPhrases == null) {
			result = false;
		} else {
			final List<Object> values = new ArrayList<>();

			for (final WhenPhrase whenPhrase : whenPhrases) {
				final List<When> whens = whenPhrase.getWhens();

				for (final When when : whens) {
					final Condition condition = when.getCondition();
					final Value value = condition.getValue();

					if (value != null) {
						final ValueStmt valueStmt = value.getValueStmt();

						if (valueStmt != null) {
							final CobolValue valueObject = valueStmtService.getValue(valueStmt, null);

							if (valueObject != null) {
								values.add(valueService.getValue(valueObject));
							}
						}
					}
				}
			}

			if (values.isEmpty()) {
				result = false;
			} else {
				final Set<Object> valueSet = new HashSet<>(values);
				result = values.size() != valueSet.size();
			}
		}

		return result;
	}

	// TODO: IF-ELSE Statements are not considered yet!
	protected boolean isRelevantStatement(final Statement statement) {
		final StatementType statementType = statement.getStatementType();
		final boolean result;

		if (!StatementTypeEnum.EVALUATE.equals(statementType)) {
			result = false;
		} else {
			final EvaluateStatement evalStatement = (EvaluateStatement) statement;

			result = isRelevantEvaluateStatement(evalStatement);
		}

		return result;
	}
}
