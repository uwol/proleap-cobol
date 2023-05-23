package io.proleap.cobol.analysis.issues.rules.procedure.subtract;

import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractStatement;
import io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractStatement.SubtractType;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class SubtractCorrespondingNotRecommendedFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final boolean isSubtractStatement = StatementTypeEnum.SUBTRACT.equals(statement.getStatementType());
		final boolean result;

		if (!isSubtractStatement) {
			result = false;
		} else {
			final SubtractStatement subtractStatement = (SubtractStatement) statement;
			result = SubtractType.CORRESPONDING.equals(subtractStatement.getSubtractType());
		}

		return result;
	}
}
