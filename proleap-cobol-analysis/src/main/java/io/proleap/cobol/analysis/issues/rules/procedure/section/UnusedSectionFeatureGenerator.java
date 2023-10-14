package io.proleap.cobol.analysis.issues.rules.procedure.section;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.SectionCall;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.DependingOnPhrase;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.GoToStatement;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.GoToStatement.GoToType;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.Simple;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformProcedureStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class UnusedSectionFeatureGenerator extends FeatureGenerator<Section> {

	protected List<Section> explicitlyCalledSections(final CompilationUnit compilationUnit) {
		final List<Section> result = new ArrayList<Section>();
		final List<Call> calls = new ArrayList<Call>();

		final List<Statement> statements = CobolStreamUtils.statementsRec(compilationUnit).collect(Collectors.toList());

		for (final Statement statement : statements) {
			if (statement != null) {
				if (statement.getStatementType().equals(StatementTypeEnum.GO_TO)) {
					final GoToStatement gotoStatement = (GoToStatement) statement;

					final GoToType goToStatementType = gotoStatement.getGoToType();
					if (GoToType.DEPENDING_ON.equals(goToStatementType)) {
						final DependingOnPhrase phrase = gotoStatement.getDependingOnPhrase();
						calls.addAll(phrase.getProcedureCalls());
					} else {
						final Simple phrase = gotoStatement.getSimple();
						calls.add(phrase.getProcedureCall());
					}
				} else if (statement.getStatementType().equals(StatementTypeEnum.PERFORM)) {
					final PerformStatement performstatement = (PerformStatement) statement;
					if (performstatement.getPerformStatementType()
							.equals(PerformStatement.PerformStatementType.PROCEDURE)) {
						final PerformProcedureStatement performProcedureStmt = performstatement
								.getPerformProcedureStatement();
						calls.addAll(performProcedureStmt.getCalls());
					}
				}
			}
		}

		for (final Call call : calls) {
			if (call.getCallType().equals(Call.CallType.SECTION_CALL)) {
				final SectionCall sectionCall = (SectionCall) call;
				result.add(sectionCall.getSection());
			}
		}
		return result;
	}

	@Override
	public Stream<Section> getAll(final CompilationUnit compilationUnit) {

		final List<Section> explicitlyCalledSections = explicitlyCalledSections(compilationUnit);

		return CobolStreamUtils.sections(compilationUnit).filter(section -> {
			return !explicitlyCalledSections.contains(section);
		});
	}
}
