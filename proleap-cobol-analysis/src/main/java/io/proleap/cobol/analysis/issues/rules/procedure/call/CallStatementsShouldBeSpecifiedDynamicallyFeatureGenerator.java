package io.proleap.cobol.analysis.issues.rules.procedure.call;

import java.util.stream.Stream;

import jakarta.inject.Singleton;

import org.antlr.v4.runtime.ParserRuleContext;

import io.proleap.cobol.CobolParser.IdentifierContext;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.call.CallStatement;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class CallStatementsShouldBeSpecifiedDynamicallyFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return (StatementTypeEnum.CALL.equals(statement.getStatementType()));
		}).filter(statement -> isRelevantStatement(statement));
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final CallStatement callStatement = (CallStatement) statement;
		final ValueStmt programValueStmt = callStatement.getProgramValueStmt();
		final ParserRuleContext ctx = programValueStmt.getCtx();

		if (ctx instanceof IdentifierContext) {
			return false;
		}

		return true;
	}
}
