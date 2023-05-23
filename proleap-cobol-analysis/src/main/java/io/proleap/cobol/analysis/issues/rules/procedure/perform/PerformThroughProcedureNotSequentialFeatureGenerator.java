package io.proleap.cobol.analysis.issues.rules.procedure.perform;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.ProcedureCall;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformProcedureStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement.PerformStatementType;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class PerformThroughProcedureNotSequentialFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isParagraphSuccessor(final Paragraph paragraph1, final Paragraph paragraph2) {
		final ProgramUnit programUnit1 = paragraph1.getProgramUnit();
		final ProgramUnit programUnit2 = paragraph2.getProgramUnit();
		final boolean result;

		if (programUnit1 != programUnit2) {
			result = false;
		} else {
			final ProcedureDivision procedureDivision = programUnit1.getProcedureDivision();
			final List<Paragraph> paragraphs = procedureDivision.getParagraphs();

			final int paragraph1Position = paragraphs.indexOf(paragraph1);
			final int paragraph2Position = paragraphs.indexOf(paragraph2);

			result = paragraph1Position < paragraph2Position;
		}

		return result;
	}

	protected boolean isRelevantPerformProcedureStatement(final PerformStatement performStatement) {
		final PerformProcedureStatement performProcedureStatement = performStatement.getPerformProcedureStatement();

		final List<Call> calls = performProcedureStatement.getCalls();
		final List<Call> procedureCalls = calls.stream().filter(call -> {
			return CallType.PROCEDURE_CALL.equals(call.getCallType());
		}).collect(Collectors.toList());

		boolean result = false;

		for (int i = 0; i < procedureCalls.size() - 1; i++) {
			final ProcedureCall procedureCall1 = (ProcedureCall) procedureCalls.get(i);
			final ProcedureCall procedureCall2 = (ProcedureCall) procedureCalls.get(i + 1);

			final Paragraph paragraph1 = procedureCall1.getParagraph();
			final Paragraph paragraph2 = procedureCall2.getParagraph();

			final boolean isSuccessor = isParagraphSuccessor(paragraph1, paragraph2);

			if (!isSuccessor) {
				result = true;
				break;
			}
		}

		return result;
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final boolean isPerformStatement = StatementTypeEnum.PERFORM.equals(statement.getStatementType());
		final boolean result;

		if (!isPerformStatement) {
			result = false;
		} else {
			final PerformStatement performStatement = (PerformStatement) statement;
			final PerformStatementType performStatementType = performStatement.getPerformStatementType();

			if (PerformStatementType.PROCEDURE.equals(performStatementType)) {
				result = isRelevantPerformProcedureStatement(performStatement);
			} else {
				result = false;
			}
		}

		return result;
	}
}
