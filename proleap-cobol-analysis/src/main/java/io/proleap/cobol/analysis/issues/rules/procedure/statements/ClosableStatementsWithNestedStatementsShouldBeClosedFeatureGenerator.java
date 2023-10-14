package io.proleap.cobol.analysis.issues.rules.procedure.statements;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.AcceptStatementContext;
import io.proleap.cobol.CobolParser.AddStatementContext;
import io.proleap.cobol.CobolParser.CallStatementContext;
import io.proleap.cobol.CobolParser.ComputeStatementContext;
import io.proleap.cobol.CobolParser.DeleteStatementContext;
import io.proleap.cobol.CobolParser.DivideStatementContext;
import io.proleap.cobol.CobolParser.EvaluateStatementContext;
import io.proleap.cobol.CobolParser.IfStatementContext;
import io.proleap.cobol.CobolParser.MultiplyStatementContext;
import io.proleap.cobol.CobolParser.PerformInlineStatementContext;
import io.proleap.cobol.CobolParser.ReadStatementContext;
import io.proleap.cobol.CobolParser.ReceiveStatementContext;
import io.proleap.cobol.CobolParser.ReturnStatementContext;
import io.proleap.cobol.CobolParser.RewriteStatementContext;
import io.proleap.cobol.CobolParser.SearchStatementContext;
import io.proleap.cobol.CobolParser.StartStatementContext;
import io.proleap.cobol.CobolParser.StringStatementContext;
import io.proleap.cobol.CobolParser.SubtractStatementContext;
import io.proleap.cobol.CobolParser.UnstringStatementContext;
import io.proleap.cobol.CobolParser.WriteStatementContext;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.accept.AcceptStatement;
import io.proleap.cobol.asg.metamodel.procedure.add.AddStatement;
import io.proleap.cobol.asg.metamodel.procedure.call.CallStatement;
import io.proleap.cobol.asg.metamodel.procedure.compute.ComputeStatement;
import io.proleap.cobol.asg.metamodel.procedure.delete.DeleteStatement;
import io.proleap.cobol.asg.metamodel.procedure.divide.DivideStatement;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.EvaluateStatement;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.IfStatement;
import io.proleap.cobol.asg.metamodel.procedure.multiply.MultiplyStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformInlineStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement.PerformStatementType;
import io.proleap.cobol.asg.metamodel.procedure.read.ReadStatement;
import io.proleap.cobol.asg.metamodel.procedure.receive.ReceiveStatement;
import io.proleap.cobol.asg.metamodel.procedure.returnstmt.ReturnStatement;
import io.proleap.cobol.asg.metamodel.procedure.rewrite.RewriteStatement;
import io.proleap.cobol.asg.metamodel.procedure.search.SearchStatement;
import io.proleap.cobol.asg.metamodel.procedure.start.StartStatement;
import io.proleap.cobol.asg.metamodel.procedure.string.StringStatement;
import io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractStatement;
import io.proleap.cobol.asg.metamodel.procedure.unstring.UnstringStatement;
import io.proleap.cobol.asg.metamodel.procedure.write.WriteStatement;
import io.proleap.cobol.commons.stream.CobolNestedStreams;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class ClosableStatementsWithNestedStatementsShouldBeClosedFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isClosedStatement(final Statement statement) {
		final boolean result;
		final StatementTypeEnum statementType = (StatementTypeEnum) statement.getStatementType();

		switch (statementType) {
		case ACCEPT:
			final AcceptStatement acceptStatement = (AcceptStatement) statement;
			final AcceptStatementContext acceptCtx = (AcceptStatementContext) acceptStatement.getCtx();
			result = (acceptCtx.END_ACCEPT() != null);
			break;
		case ADD:
			final AddStatement addStatement = (AddStatement) statement;
			final AddStatementContext addCtx = (AddStatementContext) addStatement.getCtx();
			result = (addCtx.END_ADD() != null);
			break;
		case CALL:
			final CallStatement callStatement = (CallStatement) statement;
			final CallStatementContext callCtx = (CallStatementContext) callStatement.getCtx();
			result = (callCtx.END_CALL() != null);
			break;
		case COMPUTE:
			final ComputeStatement computeStatement = (ComputeStatement) statement;
			final ComputeStatementContext computeCtx = (ComputeStatementContext) computeStatement.getCtx();
			result = (computeCtx.END_COMPUTE() != null);
			break;
		case DELETE:
			final DeleteStatement deleteStatement = (DeleteStatement) statement;
			final DeleteStatementContext deleteCtx = (DeleteStatementContext) deleteStatement.getCtx();
			result = (deleteCtx.END_DELETE() != null);
			break;
		case DIVIDE:
			final DivideStatement divideStatement = (DivideStatement) statement;
			final DivideStatementContext divideCtx = (DivideStatementContext) divideStatement.getCtx();
			result = (divideCtx.END_DIVIDE() != null);
			break;
		case EVALUATE:
			final EvaluateStatement evaluateStatement = (EvaluateStatement) statement;
			final EvaluateStatementContext evaluateCtx = (EvaluateStatementContext) evaluateStatement.getCtx();
			result = (evaluateCtx.END_EVALUATE() != null);
			break;
		case IF:
			final IfStatement ifStatement = (IfStatement) statement;
			final IfStatementContext ifCtx = (IfStatementContext) ifStatement.getCtx();
			result = (ifCtx.END_IF() != null);
			break;
		case MULTIPLY:
			final MultiplyStatement multiplyStatement = (MultiplyStatement) statement;
			final MultiplyStatementContext multiplyCtx = (MultiplyStatementContext) multiplyStatement.getCtx();
			result = (multiplyCtx.END_MULTIPLY() != null);
			break;
		case PERFORM:
			final PerformStatement performStatement = (PerformStatement) statement;
			final PerformStatementType performStatementType = performStatement.getPerformStatementType();

			if (PerformStatement.PerformStatementType.INLINE.equals(performStatementType)) {
				final PerformInlineStatement performInlineStatement = performStatement.getPerformInlineStatement();
				final PerformInlineStatementContext performCtx = (PerformInlineStatementContext) performInlineStatement
						.getCtx();
				result = (performCtx.END_PERFORM() != null);
				break;
			} else {
				result = false;
				break;
			}
		case READ:
			final ReadStatement readStatement = (ReadStatement) statement;
			final ReadStatementContext readCtx = (ReadStatementContext) readStatement.getCtx();
			result = (readCtx.END_READ() != null);
			break;
		case RECEIVE:
			final ReceiveStatement receiveStatement = (ReceiveStatement) statement;
			final ReceiveStatementContext receiveCtx = (ReceiveStatementContext) receiveStatement.getCtx();
			result = (receiveCtx.END_RECEIVE() != null);
			break;
		case RETURN:
			final ReturnStatement returnStatement = (ReturnStatement) statement;
			final ReturnStatementContext returnCtx = (ReturnStatementContext) returnStatement.getCtx();
			result = (returnCtx.END_RETURN() != null);
			break;
		case REWRITE:
			final RewriteStatement rewriteStatement = (RewriteStatement) statement;
			final RewriteStatementContext rewriteCtx = (RewriteStatementContext) rewriteStatement.getCtx();
			result = (rewriteCtx.END_REWRITE() != null);
			break;
		case SEARCH:
			final SearchStatement searchStatement = (SearchStatement) statement;
			final SearchStatementContext searchCtx = (SearchStatementContext) searchStatement.getCtx();
			result = (searchCtx.END_SEARCH() != null);
			break;
		case START:
			final StartStatement startStatement = (StartStatement) statement;
			final StartStatementContext startCtx = (StartStatementContext) startStatement.getCtx();
			result = (startCtx.END_START() != null);
			break;
		case STRING:
			final StringStatement stringStatement = (StringStatement) statement;
			final StringStatementContext stringCtx = (StringStatementContext) stringStatement.getCtx();
			result = (stringCtx.END_STRING() != null);
			break;
		case SUBTRACT:
			final SubtractStatement subtractStatement = (SubtractStatement) statement;
			final SubtractStatementContext subtractCtx = (SubtractStatementContext) subtractStatement.getCtx();
			result = (subtractCtx.END_SUBTRACT() != null);
			break;
		case UNSTRING:
			final UnstringStatement unstringStatement = (UnstringStatement) statement;
			final UnstringStatementContext unstringCtx = (UnstringStatementContext) unstringStatement.getCtx();
			result = (unstringCtx.END_UNSTRING() != null);
			break;
		case WRITE:
			final WriteStatement writeStatement = (WriteStatement) statement;
			final WriteStatementContext writeCtx = (WriteStatementContext) writeStatement.getCtx();
			result = (writeCtx.END_WRITE() != null);
			break;
		default:
			result = false;
			break;
		}

		return result;
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final boolean result;
		final List<Statement> nestedStatements = CobolNestedStreams.nestedStatements(statement)
				.collect(Collectors.toList());

		if (nestedStatements.isEmpty()) {
			result = false;
		} else {
			result = !isClosedStatement(statement);
		}

		return result;
	}
}
