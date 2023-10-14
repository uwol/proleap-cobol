package io.proleap.cobol.interpreter.handlers.procedure.impl;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import jakarta.inject.Singleton;

import org.antlr.v4.runtime.CommonTokenStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.interpreter.domain.scope.CobolInterpreterScope;
import io.proleap.cobol.interpreter.exception.CobolInterpreterException;
import io.proleap.cobol.interpreter.handlers.impl.AbstractCobolInterpreterHandler;
import io.proleap.cobol.interpreter.handlers.procedure.StatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.StatementsHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.preprocessor.sub.util.TokenUtils;

@Singleton
public class StatementsHandlerImpl extends AbstractCobolInterpreterHandler implements StatementsHandler {

	public class StatementPrinter {

		protected Statement statement;

		public StatementPrinter(final Statement statement) {
			this.statement = statement;
		}

		@Override
		public String toString() {
			final CommonTokenStream tokens = statement.getCompilationUnit().getTokens();
			final String result = TokenUtils.getTextIncludingHiddenTokens(statement.getCtx(), tokens);
			return result.trim().replaceAll("[ ]{2,}", " ").replaceAll("[\r\n]", "");
		}
	}

	private final static Logger LOG = LoggerFactory.getLogger(StatementsHandlerImpl.class);

	protected Map<StatementType, StatementHandler<?>> handlers = new HashMap<>();

	@Override
	public void registerStatement(final StatementHandler<?> statementHandler) {
		final StatementType statementType = statementHandler.getStatementType();

		if (handlers.get(statementType) != null) {
			throw new CobolInterpreterException("Statement handler for " + statementType + " is not null");
		}

		handlers.put(statementHandler.getStatementType(), statementHandler);
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public void run(final List<Statement> statements, final CobolInterpreterParams params) {
		final Iterator<Statement> iterator = statements.iterator();
		final CobolInterpreterScope scope = params.getState().getStatementsHandlerScopes().push();

		while (!scope.isHalted() && !params.getState().isHalted() && iterator.hasNext()) {
			final Statement statement = iterator.next();
			final StatementType statementType = statement.getStatementType();
			final StatementHandler statementHandler = handlers.get(statementType);

			if (params.getState().getOps() > OPS_LIMIT) {
				throw new CobolInterpreterException("Exceeded ops limit of " + OPS_LIMIT);
			} else if (statementHandler == null) {
				throw new CobolInterpreterException("Missing statement handler for " + statementType);
			} else {
				final StatementPrinter printer = new StatementPrinter(statement);
				LOG.info("{}", printer);

				statementHandler.run(statement, params);
				params.getState().incOps();
			}
		}

		params.getState().getStatementsHandlerScopes().pop();
	}
}
