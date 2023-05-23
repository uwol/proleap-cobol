package io.proleap.cobol.interpreter.handlers.procedure;

import java.util.List;

import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public interface StatementsHandler {

	static final int OPS_LIMIT = 100000;

	void registerStatement(StatementHandler<?> statementHandler);

	void run(List<Statement> statements, CobolInterpreterParams params);
}
