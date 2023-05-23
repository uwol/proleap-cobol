package io.proleap.cobol.interpreter.handlers.procedure;

import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.interpreter.context.PostConstructBean;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public interface StatementHandler<S extends Statement> extends PostConstructBean {

	StatementType getStatementType();

	void run(S statement, CobolInterpreterParams params);
}
