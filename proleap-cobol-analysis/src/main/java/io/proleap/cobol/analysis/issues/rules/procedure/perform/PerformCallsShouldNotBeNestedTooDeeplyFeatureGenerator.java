package io.proleap.cobol.analysis.issues.rules.procedure.perform;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.ProcedureCall;
import io.proleap.cobol.asg.metamodel.call.SectionCall;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformProcedureStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class PerformCallsShouldNotBeNestedTooDeeplyFeatureGenerator extends FeatureGenerator<Statement> {

	private static final int MAX_PERFORM_DEPTH = 3;

	protected List<PerformStatement> containedPerformStmts(final Paragraph paragraph) {
		final List<Statement> statements = paragraph.getStatements();
		final List<PerformStatement> performStatements = new ArrayList<>();

		for (final Statement statement : statements) {
			if (statement.getStatementType().equals(StatementTypeEnum.PERFORM)) {
				final PerformStatement performStatement = (PerformStatement) statement;

				if (performStatement.getPerformStatementType()
						.equals(PerformStatement.PerformStatementType.PROCEDURE)) {
					performStatements.add(performStatement);
				}
			}
		}

		return performStatements;
	}

	protected List<PerformStatement> containedPerformStmts(final Section section) {
		final List<Statement> statements = section.getStatements();
		final List<PerformStatement> performStatements = new ArrayList<>();

		for (final Statement statement : statements) {
			if (statement.getStatementType().equals(StatementTypeEnum.PERFORM)) {
				final PerformStatement performStatement = (PerformStatement) statement;

				if (performStatement.getPerformStatementType()
						.equals(PerformStatement.PerformStatementType.PROCEDURE)) {
					performStatements.add(performStatement);
				}
			}
		}

		return performStatements;
	}

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		final List<Statement> result = new ArrayList<>();
		final List<Statement> list = CobolStreamUtils.statementsRec(compilationUnit).collect(Collectors.toList());

		for (final Statement statement : list) {
			if (statement.getStatementType().equals(StatementTypeEnum.PERFORM)) {
				final PerformStatement performstatement = (PerformStatement) statement;
				final PerformStatement recDepth = recDepth(performstatement, new ArrayList<ASGElement>(), 0);

				if (recDepth != null) {
					result.add(recDepth);
				}
			}
		}

		return result.stream();
	}

	protected PerformStatement recDepth(final PerformStatement performStatement, final List<ASGElement> list, int lvl) {
		if (lvl >= MAX_PERFORM_DEPTH) {
			return performStatement;
		}

		final PerformProcedureStatement performProcedureStatement = performStatement.getPerformProcedureStatement();

		if (performProcedureStatement != null) {
			final List<Call> calls = performProcedureStatement.getCalls();

			for (final Call call : calls) {
				if (call.getCallType().equals(CallType.PROCEDURE_CALL)) {
					final ProcedureCall procedureCall = (ProcedureCall) call;
					final Paragraph paragraph = procedureCall.getParagraph();

					if (list.contains(paragraph) || containedPerformStmts(paragraph).isEmpty()) {
					} else {
						for (final Statement statement : paragraph.getStatements()) {
							if (statement.getStatementType().equals(StatementTypeEnum.PERFORM)) {
								list.add(paragraph);

								return recDepth((PerformStatement) statement, list, ++lvl);
							}
						}

					}
				} else if (call.getCallType().equals(CallType.SECTION_CALL)) {
					final SectionCall sectionCall = (SectionCall) call;
					final Section section = sectionCall.getSection();

					if (list.contains(section) || containedPerformStmts(section).isEmpty()) {
					} else {
						for (final Statement statement : section.getStatements()) {
							if (statement.getStatementType().equals(StatementTypeEnum.PERFORM)) {
								list.add(section);

								return recDepth((PerformStatement) statement, list, ++lvl);
							}
						}

					}
				}
			}
		}
		return null;
	}
}