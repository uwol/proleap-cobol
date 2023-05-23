package io.proleap.cobol.analysis.issues.rules.procedure.move;

import java.util.stream.Stream;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveStatement;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveToStatement;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class MoveOnlyToExistingVariablesFeatureGenerator extends FeatureGenerator<Statement> {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final boolean isMoveStatement = StatementTypeEnum.MOVE.equals(statement.getStatementType());
		boolean result = false;

		if (!isMoveStatement) {
			result = false;
		} else {
			final MoveStatement moveStatement = (MoveStatement) statement;
			final boolean isMoveToStatement = MoveStatement.MoveType.MOVE_TO.equals(moveStatement.getMoveType());

			if (!isMoveToStatement) {
				result = false;
			} else {
				final MoveToStatement moveToStatement = moveStatement.getMoveToStatement();

				for (final Call call : moveToStatement.getReceivingAreaCalls()) {
					if (!Call.CallType.DATA_DESCRIPTION_ENTRY_CALL.equals(call.getCallType())
							&& !Call.CallType.UNDEFINED_CALL.equals(call.getCallType())) {
						return false;
					}

					final DataDescriptionEntry dataDescriptionEntry = cobolDataDescriptionEntryService
							.getDataDescriptionEntry(call);

					if (dataDescriptionEntry == null) {
						result = true;
						break;
					}
				}
			}
		}

		return result;
	}
}
