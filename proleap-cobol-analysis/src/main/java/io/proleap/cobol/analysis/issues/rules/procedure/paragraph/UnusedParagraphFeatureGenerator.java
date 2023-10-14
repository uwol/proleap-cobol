package io.proleap.cobol.analysis.issues.rules.procedure.paragraph;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.ProcedureCall;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.DependingOnPhrase;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.GoToStatement;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.Simple;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformProcedureStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class UnusedParagraphFeatureGenerator extends FeatureGenerator<Paragraph> {

	protected List<Paragraph> explicitlyCalledParagraphs(final CompilationUnit compilationUnit) {
		final List<Statement> statements = CobolStreamUtils.statementsRec(compilationUnit).collect(Collectors.toList());
		final List<Paragraph> result = new ArrayList<Paragraph>();

		for (final Statement statement : statements) {
			if (StatementTypeEnum.GO_TO.equals(statement.getStatementType())) {
				final GoToStatement gotoStatement = (GoToStatement) statement;

				if (GoToStatement.GoToType.SIMPLE.equals(gotoStatement.getGoToType())) {
					final Simple simple = gotoStatement.getSimple();
					final Call call = simple.getProcedureCall();

					if (Call.CallType.PROCEDURE_CALL.equals(call.getCallType())) {
						final ProcedureCall procedureCall = (ProcedureCall) call;
						final Paragraph paragraph = procedureCall.getParagraph();
						result.add(paragraph);
					}
				} else if (gotoStatement.getGoToType().equals(GoToStatement.GoToType.DEPENDING_ON)) {
					final DependingOnPhrase dependingOnPhrase = gotoStatement.getDependingOnPhrase();
					final List<Call> procedureCalls = dependingOnPhrase.getProcedureCalls();

					for (final Call call : procedureCalls) {
						if (Call.CallType.PROCEDURE_CALL.equals(call.getCallType())) {
							final ProcedureCall procedureCall = (ProcedureCall) call;
							result.add(procedureCall.getParagraph());
						}
					}
				}
			} else if (statement.getStatementType().equals(StatementTypeEnum.PERFORM)) {
				final PerformStatement performStatement = (PerformStatement) statement;
				final PerformProcedureStatement performProcedureStatement = performStatement
						.getPerformProcedureStatement();

				if (performProcedureStatement != null) {
					final List<Call> calls = performProcedureStatement.getCalls();

					for (final Call call : calls) {
						if (call.getCallType().equals(Call.CallType.PROCEDURE_CALL)) {
							final ProcedureCall procedureCall = (ProcedureCall) call;
							final Paragraph paragraph = procedureCall.getParagraph();
							result.add(paragraph);
						}
					}
				}
			}
		}

		return result;
	}

	@Override
	public Stream<Paragraph> getAll(final CompilationUnit compilationUnit) {
		final List<Paragraph> explicitlyCalledParagraphs = explicitlyCalledParagraphs(compilationUnit);

		return CobolStreamUtils.paragraphs(compilationUnit).filter(paragraph -> {
			return !explicitlyCalledParagraphs.contains(paragraph);
		});
	}
}
