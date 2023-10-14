package io.proleap.cobol.analysis.issues.rules.procedure.call;

import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.call.CallStatement;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.util.CobolStreamUtils;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;

@Singleton
public class NumberOfSubprogramsCalledShouldBeLimitedFeatureGenerator extends FeatureGenerator<ProgramUnit> {

	final static int MAX_SUBPROGRAMS = 10;

	@Inject
	private CobolValueStmtService valueStmtService;

	@Override
	public Stream<ProgramUnit> getAll(final CompilationUnit compilationUnit) {
		final List<CallStatement> callStatements = CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return StatementTypeEnum.CALL.equals(statement.getStatementType());
		}).map(statement -> {
			return (CallStatement) statement;
		}).collect(Collectors.toList());

		final int numSubprograms = getNumberOfSubprograms(callStatements);

		if (numSubprograms > MAX_SUBPROGRAMS) {
			return Stream.of(compilationUnit.getProgramUnit());
		} else {
			return Stream.empty();
		}
	}

	protected int getNumberOfSubprograms(final List<CallStatement> callStatements) {
		final HashSet<CobolValue> literalSet = new HashSet<>();

		for (final CallStatement callStatement : callStatements) {
			final ValueStmt programValueStmt = callStatement.getProgramValueStmt();

			if (programValueStmt != null) {
				final CobolValue value = valueStmtService.getValue(programValueStmt, null);

				if (value != null) {
					literalSet.add(value);
				}
			}
		}

		return literalSet.size();
	}
}