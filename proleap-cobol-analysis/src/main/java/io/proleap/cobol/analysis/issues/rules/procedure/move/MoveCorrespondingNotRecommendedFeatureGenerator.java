package io.proleap.cobol.analysis.issues.rules.procedure.move;

import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveStatement;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveStatement.MoveType;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class MoveCorrespondingNotRecommendedFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final boolean isMoveStatement = StatementTypeEnum.MOVE.equals(statement.getStatementType());
		final boolean result;

		if (!isMoveStatement) {
			result = false;
		} else {
			final MoveStatement moveStatement = (MoveStatement) statement;
			result = MoveType.MOVE_CORRESPONDING.equals(moveStatement.getMoveType());
		}

		return result;
	}
}
