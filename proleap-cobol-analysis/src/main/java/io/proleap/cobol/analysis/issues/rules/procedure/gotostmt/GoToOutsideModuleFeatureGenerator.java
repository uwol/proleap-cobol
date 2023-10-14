package io.proleap.cobol.analysis.issues.rules.procedure.gotostmt;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.ProcedureCall;
import io.proleap.cobol.asg.metamodel.call.SectionCall;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.DependingOnPhrase;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.GoToStatement;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.GoToStatement.GoToType;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.Simple;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformProcedureStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement.PerformStatementType;
import io.proleap.cobol.asg.util.ANTLRUtils;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class GoToOutsideModuleFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		final Stream<Statement> sectionStatements = CobolStreamUtils.sections(compilationUnit)
				.map(section -> isRelevantSection(section)).flatMap(Function.identity());

		final Stream<Statement> performThroughStatement = CobolStreamUtils.statementsRec(compilationUnit)
				.filter(statement -> StatementTypeEnum.PERFORM.equals(statement.getStatementType()))
				.filter(statement -> PerformStatementType.PROCEDURE
						.equals(((PerformStatement) statement).getPerformStatementType()))
				.map(statement -> isRelevantPerformThrough(
						((PerformStatement) statement).getPerformProcedureStatement()))
				.flatMap(Function.identity());

		return Stream.concat(sectionStatements, performThroughStatement);
	}

	protected boolean isRelevantGoTo(final Statement statement, final List<Paragraph> module) {
		final GoToStatement goToStmt = (GoToStatement) statement;
		final GoToType goToStatementType = goToStmt.getGoToType();
		final List<Call> calls = new ArrayList<Call>();

		if (GoToType.DEPENDING_ON.equals(goToStatementType)) {
			final DependingOnPhrase phrase = goToStmt.getDependingOnPhrase();
			calls.addAll(phrase.getProcedureCalls());
		} else {
			final Simple phrase = goToStmt.getSimple();
			calls.add(phrase.getProcedureCall());
		}

		boolean result = false;

		for (final Call call : calls) {
			if (CallType.SECTION_CALL.equals(call.getCallType())) {
				result = true;
				break;
			}

			if (CallType.PROCEDURE_CALL.equals(call.getCallType())) {
				final ProcedureCall procedureCall = (ProcedureCall) call;
				final Paragraph paragraph = procedureCall.getParagraph();

				if (!module.contains(paragraph)) {
					result = true;
					break;
				}
			}
		}

		return result;
	}

	protected boolean isRelevantGoTo(final Statement statement, final Section section) {
		final GoToStatement goToStmt = (GoToStatement) statement;
		final GoToType goToStatementType = goToStmt.getGoToType();
		final List<Call> calls = new ArrayList<Call>();

		if (GoToType.DEPENDING_ON.equals(goToStatementType)) {
			final DependingOnPhrase phrase = goToStmt.getDependingOnPhrase();
			calls.addAll(phrase.getProcedureCalls());
		} else {
			final Simple phrase = goToStmt.getSimple();
			calls.add(phrase.getProcedureCall());
		}

		boolean result = false;

		for (final Call call : calls) {
			if (!isSameSection(section, call)) {
				result = true;
				break;
			}
		}

		return result;
	}

	protected Stream<Statement> isRelevantPerformThrough(final PerformProcedureStatement performProcedureStatement) {
		final List<Statement> result = new ArrayList<Statement>();
		final List<Call> calls = performProcedureStatement.getCalls();

		if (calls.size() > 1) {
			final List<Paragraph> module = calls.stream()
					.filter(call -> CallType.PROCEDURE_CALL.equals(call.getCallType()))
					.map(call -> ((ProcedureCall) call).getParagraph()).collect(Collectors.toList());

			for (final Call call : calls) {
				if (CallType.PROCEDURE_CALL.equals(call.getCallType())) {
					final ProcedureCall procedureCall = (ProcedureCall) call;
					final Paragraph paragraph = procedureCall.getParagraph();
					final Stream<Statement> statements = paragraph.getStatements().stream()
							.filter(statement -> StatementTypeEnum.GO_TO.equals(statement.getStatementType()))
							.filter(statement -> isRelevantGoTo(statement, module));
					result.addAll(statements.collect(Collectors.toList()));
				}
			}
		}

		return result.stream();
	}

	protected Stream<Statement> isRelevantSection(final Section section) {
		final List<Statement> list = new ArrayList<Statement>();

		for (final Statement statement : section.getStatements()) {
			if (isRelevantStatement(statement, section)) {
				list.add(statement);
			}
		}

		return list.stream();
	}

	protected boolean isRelevantStatement(final Statement statement, final Section section) {
		final boolean result;

		if (StatementTypeEnum.GO_TO.equals(statement.getStatementType())) {
			result = isRelevantGoTo(statement, section);
		} else {
			result = false;
		}

		return result;
	}

	protected boolean isSameSection(final Section section, final Call call) {
		if (CallType.SECTION_CALL.equals(call.getCallType())) {
			final SectionCall sectionCall = (SectionCall) call;
			return (section.equals(sectionCall.getSection()));
		} else if (CallType.PROCEDURE_CALL.equals(call.getCallType())) {
			final ProcedureCall procedureCall = (ProcedureCall) call;
			final Paragraph paragraph = procedureCall.getParagraph();
			final Section sec = ANTLRUtils.findParent(Section.class, paragraph.getCtx(),
					call.getProgram().getASGElementRegistry());
			return (section.equals(sec));
		} else {
			return false;
		}
	}
}
