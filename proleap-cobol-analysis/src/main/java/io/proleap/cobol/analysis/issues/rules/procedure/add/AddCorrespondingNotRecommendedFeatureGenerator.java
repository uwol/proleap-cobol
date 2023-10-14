package io.proleap.cobol.analysis.issues.rules.procedure.add;

import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.add.AddStatement;
import io.proleap.cobol.asg.metamodel.procedure.add.AddStatement.AddType;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class AddCorrespondingNotRecommendedFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final boolean isAddStatement = StatementTypeEnum.ADD.equals(statement.getStatementType());
		final boolean result;

		if (!isAddStatement) {
			result = false;
		} else {
			final AddStatement addStatement = (AddStatement) statement;
			result = AddType.CORRESPONDING.equals(addStatement.getAddType());
		}

		return result;
	}
}
