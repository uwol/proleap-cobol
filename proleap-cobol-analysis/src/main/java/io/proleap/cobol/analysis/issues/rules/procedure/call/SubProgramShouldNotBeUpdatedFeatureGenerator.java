package io.proleap.cobol.analysis.issues.rules.procedure.call;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.call.CallStatement;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveStatement;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class SubProgramShouldNotBeUpdatedFeatureGenerator extends FeatureGenerator<Statement> {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return (StatementTypeEnum.CALL.equals(statement.getStatementType()));
		}).filter(statement -> isRelevantStatement(statement, compilationUnit));
	}

	protected List<Call> getAllReceivingAreaCalls(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit)
				.filter(streamStatement -> StatementTypeEnum.MOVE.equals(streamStatement.getStatementType()))
				.filter(streamStatement -> MoveStatement.MoveType.MOVE_TO
						.equals(((MoveStatement) streamStatement).getMoveType()))
				.map(streamStatement -> ((MoveStatement) streamStatement).getMoveToStatement().getReceivingAreaCalls())
				.flatMap(List::stream).collect(Collectors.toList());
	}

	protected boolean isRelevantStatement(final Statement statement, final CompilationUnit compilationUnit) {
		final CallStatement callStatement = (CallStatement) statement;
		final ValueStmt valueStmt = callStatement.getProgramValueStmt();
		final DataDescriptionEntry dataDescriptionEntry = cobolDataDescriptionEntryService
				.getDataDescriptionEntry(valueStmt);

		boolean result = false;

		if (dataDescriptionEntry == null) {
		} else {
			for (final Call receivingAreaCall : getAllReceivingAreaCalls(compilationUnit)) {
				final DataDescriptionEntry receivingDataDescriptionEntry = cobolDataDescriptionEntryService
						.getDataDescriptionEntry(receivingAreaCall);

				if (dataDescriptionEntry.equals(receivingDataDescriptionEntry)) {
					result = true;
					break;
				}
			}
		}

		return result;
	}
}
