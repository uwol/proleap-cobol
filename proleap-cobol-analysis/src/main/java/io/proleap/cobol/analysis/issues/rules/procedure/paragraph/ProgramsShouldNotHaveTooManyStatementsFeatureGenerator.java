package io.proleap.cobol.analysis.issues.rules.procedure.paragraph;

import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class ProgramsShouldNotHaveTooManyStatementsFeatureGenerator extends FeatureGenerator<ProcedureDivision> {

	final static int MAX_STATEMENTS = 100;

	@Override
	public Stream<ProcedureDivision> getAll(final CompilationUnit compilationUnit) {
		final ProcedureDivision procedureDivision = compilationUnit.getProgramUnit().getProcedureDivision();
		final Stream<ProcedureDivision> procedureDivisionStream = Stream.of(procedureDivision);
		final Stream<Statement> statementsRec = CobolStreamUtils.statementsRec(compilationUnit);

		if (statementsRec.count() > MAX_STATEMENTS) {
			return procedureDivisionStream;
		}

		return Stream.empty();
	}
}
