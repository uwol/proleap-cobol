package io.proleap.cobol.analysis.issues.rules.procedure.stop;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class StopRunGoBackExitProgramNotLastStatementFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		final Stream<Statement> procedureDivisionStatementStream = CobolStreamUtils.procedureDivisions(compilationUnit)
				.flatMap(procedureDivision -> {
					return getRelevantStatements(procedureDivision.getStatements()).stream();
				});

		final Stream<Statement> paragraphStatementStream = CobolStreamUtils.paragraphs(compilationUnit)
				.flatMap(paragraph -> {
					return getRelevantStatements(paragraph.getStatements()).stream();
				});

		final Stream<Statement> sectionStatementStream = CobolStreamUtils.sections(compilationUnit).flatMap(section -> {
			return getRelevantStatements(section.getStatements()).stream();
		});

		return Stream.concat(procedureDivisionStatementStream,
				Stream.concat(paragraphStatementStream, sectionStatementStream));
	}

	protected List<Statement> getRelevantStatements(final List<Statement> statements) {
		final List<Statement> result = new ArrayList<Statement>();

		if (statements.isEmpty()) {
		} else {
			final List<Statement> statementsSubList = statements.subList(0, statements.size() - 1);

			for (final Statement statement : statementsSubList) {
				if (isStopRunExitStatement(statement)) {
					result.add(statement);
				}
			}
		}

		return result;
	}

	protected boolean isStopRunExitStatement(final Statement statement) {
		return StatementTypeEnum.STOP.equals(statement.getStatementType())
				|| StatementTypeEnum.GO_BACK.equals(statement.getStatementType())
				|| StatementTypeEnum.EXIT.equals(statement.getStatementType());
	}
}
