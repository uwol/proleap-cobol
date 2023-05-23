package io.proleap.cobol.analysis.issues.rules.procedure.perform;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.ProcedureCall;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformProcedureStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement.PerformStatementType;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class PerformRecursiveFeatureGenerator extends FeatureGenerator<Paragraph> {

	protected void findRecursiveCalls(final Paragraph paragraph, final List<Paragraph> pastParagraphs,
			final Set<Paragraph> recursiveParagraphSet,
			final Hashtable<Paragraph, List<Paragraph>> reachableParagraphs) {
		if (pastParagraphs.contains(paragraph)) {
			recursiveParagraphSet.addAll(pastParagraphs);
		} else {
			final List<Paragraph> calledParagraphs = reachableParagraphs.get(paragraph);

			pastParagraphs.add(paragraph);
			if (calledParagraphs != null) {
				for (final Paragraph calledParagraph : calledParagraphs) {
					findRecursiveCalls(calledParagraph, pastParagraphs, recursiveParagraphSet, reachableParagraphs);
				}
			}
		}
	}

	@Override
	public Stream<Paragraph> getAll(final CompilationUnit compilationUnit) {
		final List<Paragraph> paragraphs = CobolStreamUtils.paragraphs(compilationUnit).collect(Collectors.toList());
		final Hashtable<Paragraph, List<Paragraph>> reachableParagraphs = new Hashtable<Paragraph, List<Paragraph>>();
		final Set<Paragraph> recursiveParagraphSet = new HashSet<Paragraph>();

		for (final Paragraph paragraph : paragraphs) {
			processPerformStatements(paragraph, reachableParagraphs);
		}
		for (final Paragraph paragraph : paragraphs) {
			findRecursiveCalls(paragraph, new ArrayList<Paragraph>(), recursiveParagraphSet, reachableParagraphs);
		}

		return recursiveParagraphSet.stream();
	}

	protected void getReceivingParagraphs(final Paragraph paragraph, final PerformStatement performStatement,
			final Hashtable<Paragraph, List<Paragraph>> reachableParagraphs) {
		final PerformStatementType performStatementType = performStatement.getPerformStatementType();

		if (PerformStatementType.PROCEDURE.equals(performStatementType)) {
			final PerformProcedureStatement performProcedureStatement = performStatement.getPerformProcedureStatement();
			final List<Call> calls = performProcedureStatement.getCalls();

			final List<Paragraph> receivingParagraphs = calls.stream().filter(call -> {
				return CallType.PROCEDURE_CALL.equals(call.getCallType());
			}).map(call -> {
				final ProcedureCall procedureCall = (ProcedureCall) call;
				return procedureCall.getParagraph();
			}).collect(Collectors.toList());

			reachableParagraphs.put(paragraph, receivingParagraphs);
		}
	}

	protected void processPerformStatements(final Paragraph paragraph,
			final Hashtable<Paragraph, List<Paragraph>> reachableParagraphs) {
		final List<Statement> statements = paragraph.getStatements();

		for (final Statement statement : statements) {
			final StatementType statementType = statement.getStatementType();

			if (StatementTypeEnum.PERFORM.equals(statementType)) {
				final PerformStatement performStatement = (PerformStatement) statement;

				getReceivingParagraphs(paragraph, performStatement, reachableParagraphs);
			}
		}
	}
}
