package io.proleap.cobol.interpreter.handlers.procedure.generate.impl;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.generate.GenerateStatement;
import io.proleap.cobol.interpreter.handlers.procedure.generate.GenerateStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class GenerateStatementHandlerImpl extends StatementHandlerImpl<GenerateStatement>
		implements GenerateStatementHandler {

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.GENERATE;
	}

	@Override
	public void run(final GenerateStatement statement, final CobolInterpreterParams params) {
		// TODO report: implement Report Writer; The COBOL GENERATE statement is an
		// optional statement supported if the compiler includes the Report Writer
		// feature.
	}
}
