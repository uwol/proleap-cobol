package io.proleap.cobol.interpreter.handlers.procedure.search.impl;

import javax.annotation.PostConstruct;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.search.SearchStatement;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.search.SearchStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class SearchStatementHandlerImpl extends StatementHandlerImpl<SearchStatement>
		implements SearchStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.SEARCH;
	}

	@Override
	public void run(final SearchStatement statement, final CobolInterpreterParams params) {
		// FIXME storage: Implement OCCURS in storage area and implement SEARCH
		// statement. The SEARCH statement searches a table for an element that
		// satisfies the specified condition and adjusts the associated index to
		// indicate that element.
	}
}
