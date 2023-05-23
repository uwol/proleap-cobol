package io.proleap.cobol.interpreter.handlers.procedure.perform.impl;

import java.math.BigDecimal;
import java.util.List;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.ProcedureCall;
import io.proleap.cobol.asg.metamodel.call.SectionCall;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.perform.ByPhrase;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformInlineStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformProcedureStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement.PerformStatementType;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformType;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformType.PerformTypeType;
import io.proleap.cobol.asg.metamodel.procedure.perform.TestClause;
import io.proleap.cobol.asg.metamodel.procedure.perform.TestClause.TestClauseType;
import io.proleap.cobol.asg.metamodel.procedure.perform.Times;
import io.proleap.cobol.asg.metamodel.procedure.perform.Until;
import io.proleap.cobol.asg.metamodel.procedure.perform.Varying;
import io.proleap.cobol.asg.metamodel.procedure.perform.VaryingClause;
import io.proleap.cobol.asg.metamodel.procedure.perform.VaryingPhrase;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.impl.CobolDecimalValueImpl;
import io.proleap.cobol.interpreter.domain.scope.CobolInterpreterScope;
import io.proleap.cobol.interpreter.handlers.procedure.ParagraphHandler;
import io.proleap.cobol.interpreter.handlers.procedure.SectionHandler;
import io.proleap.cobol.interpreter.handlers.procedure.StatementsHandler;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.handlers.procedure.perform.PerformStatementHandler;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class PerformStatementHandlerImpl extends StatementHandlerImpl<PerformStatement>
		implements PerformStatementHandler {

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolStorageService storageService;

	@Inject
	private ParagraphHandler paragraphHandler;

	@Inject
	private SectionHandler sectionHandler;

	@Inject
	private StatementsHandler statementsHandler;

	@Inject
	private CobolValueService valueService;

	@Inject
	private CobolValueStmtService valueStmtService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	protected boolean checkUntil(final Until until, final CobolInterpreterParams params) {
		final CobolValue value = valueStmtService.getValue(until.getCondition(), params.getState().getStorage());
		return Boolean.TRUE.equals(valueService.getBoolean(value));
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.PERFORM;
	}

	protected void run(final PerformInlineStatement performInlineStatement, final CobolInterpreterParams params) {
		final PerformType performType = performInlineStatement.getPerformType();
		final PerformTypeType performTypeType = performType.getPerformTypeType();
		final List<Statement> statements = performInlineStatement.getStatements();

		switch (performTypeType) {
		case TIMES:
			final Times times = performType.getTimes();
			run(times, statements, params);
			break;
		case UNTIL:
			final Until until = performType.getUntil();
			run(until, statements, params);
			break;
		case VARYING:
			final Varying varying = performType.getVarying();
			run(varying, statements, params);
			break;
		}
	}

	protected void run(final PerformProcedureStatement performProcedureStatement, final CobolInterpreterParams params) {
		final CobolInterpreterScope scope = params.getState().getPerformScopes().push();

		for (final Call call : performProcedureStatement.getCalls()) {
			if (scope.isHalted()) {
				break;
			} else {
				final PerformType performType = performProcedureStatement.getPerformType();

				if (performType == null) {
					runCall(call, params);
				} else {
					final PerformTypeType performTypeType = performType.getPerformTypeType();

					switch (performTypeType) {
					case TIMES:
						final Times times = performType.getTimes();
						run(times, call, params);
						break;
					case UNTIL:
						final Until until = performType.getUntil();
						run(until, call, params);
						break;
					case VARYING:
						final Varying varying = performType.getVarying();
						run(varying, call, params);
						break;
					}
				}
			}
		}

		params.getState().getPerformScopes().pop();
	}

	@Override
	public void run(final PerformStatement statement, final CobolInterpreterParams params) {
		final PerformStatementType performStatementType = statement.getPerformStatementType();

		switch (performStatementType) {
		case INLINE:
			run(statement.getPerformInlineStatement(), params);
			break;
		case PROCEDURE:
			run(statement.getPerformProcedureStatement(), params);
			break;
		}
	}

	protected void run(final Times times, final Call procedureCall, final CobolInterpreterParams params) {
		final BigDecimal timeValue = valueService
				.getAsDecimal(valueStmtService.getValue(times.getTimesValueStmt(), params.getState().getStorage()));

		for (BigDecimal i = BigDecimal.ZERO; i.compareTo(timeValue) < 0; i = i.add(BigDecimal.ONE)) {
			runCall(procedureCall, params);
		}
	}

	protected void run(final Times times, final List<Statement> statements, final CobolInterpreterParams params) {
		final BigDecimal timeValue = valueService
				.getAsDecimal(valueStmtService.getValue(times.getTimesValueStmt(), params.getState().getStorage()));

		for (BigDecimal i = BigDecimal.ZERO; i.compareTo(timeValue) < 0; i = i.add(BigDecimal.ONE)) {
			statementsHandler.run(statements, params);
		}
	}

	protected void run(final Until until, final Call call, final CobolInterpreterParams params) {
		final TestClause testClause = until.getTestClause();

		if (testClause == null) {
			while (!checkUntil(until, params)) {
				runCall(call, params);
			}
		} else {
			final TestClauseType testClauseType = testClause.getTestClauseType();

			switch (testClauseType) {
			case AFTER:
				do {
					runCall(call, params);
				} while (!checkUntil(until, params));
				break;
			case BEFORE:
				while (!checkUntil(until, params)) {
					runCall(call, params);
				}
				break;
			}
		}
	}

	protected void run(final Until until, final List<Statement> statements, final CobolInterpreterParams params) {
		final TestClause testClause = until.getTestClause();

		if (testClause == null) {
			while (!checkUntil(until, params)) {
				statementsHandler.run(statements, params);
			}
		} else {
			final TestClauseType testClauseType = testClause.getTestClauseType();

			switch (testClauseType) {
			case AFTER:
				do {
					statementsHandler.run(statements, params);
				} while (!checkUntil(until, params));
				break;
			case BEFORE:
				while (!checkUntil(until, params)) {
					statementsHandler.run(statements, params);
				}
				break;
			}
		}
	}

	protected void run(final Varying varying, final Call procedureCall, final CobolInterpreterParams params) {
		final VaryingClause varyingClause = varying.getVaryingClause();
		final VaryingPhrase varyingPhrase = varyingClause.getVaryingPhrase();
		run(varyingPhrase, params);

		final Until until = varyingPhrase.getUntil();
		final TestClause testClause = varying.getTestClause();

		if (testClause == null) {
			while (!checkUntil(until, params)) {
				runCall(procedureCall, params);
			}
		} else {
			final TestClauseType testClauseType = testClause.getTestClauseType();

			switch (testClauseType) {
			case AFTER:
				do {
					runCall(procedureCall, params);
				} while (!checkUntil(until, params));
				break;
			case BEFORE:
				while (!checkUntil(until, params)) {
					runCall(procedureCall, params);
				}
				break;
			}
		}
	}

	protected void run(final Varying varying, final List<Statement> statements, final CobolInterpreterParams params) {
		final VaryingClause varyingClause = varying.getVaryingClause();
		final VaryingPhrase varyingPhrase = varyingClause.getVaryingPhrase();
		run(varyingPhrase, params);

		final Until until = varyingPhrase.getUntil();
		final TestClause testClause = varying.getTestClause();

		if (testClause == null) {
			while (!checkUntil(until, params)) {
				statementsHandler.run(statements, params);
			}
		} else {
			final TestClauseType testClauseType = testClause.getTestClauseType();

			switch (testClauseType) {
			case AFTER:
				do {
					statementsHandler.run(statements, params);
				} while (!checkUntil(until, params));
				break;
			case BEFORE:
				while (!checkUntil(until, params)) {
					statementsHandler.run(statements, params);
				}
				break;
			}
		}
	}

	protected void run(final VaryingPhrase varyingPhrase, final CobolInterpreterParams params) {
		final DataDescriptionEntry varyingDataDescriptionEntry = dataDescriptionEntryService
				.getDataDescriptionEntry(varyingPhrase.getVaryingValueStmt());
		final CobolValue varyingValue = storageService.getValue(varyingDataDescriptionEntry,
				params.getState().getStorage());
		final BigDecimal varyingBigDecimal = valueService.getAsDecimal(varyingValue);

		if (varyingBigDecimal != null) {
			final ByPhrase by = varyingPhrase.getBy();
			final BigDecimal byValue = valueService
					.getAsDecimal(valueStmtService.getValue(by.getByValueStmt(), params.getState().getStorage()));
			final BigDecimal varyingResult = varyingBigDecimal.add(byValue);

			storageService.putValue(varyingDataDescriptionEntry, CobolDecimalValueImpl.of(varyingResult),
					params.getState().getStorage());
		}
	}

	protected void runCall(final Call call, final CobolInterpreterParams params) {
		final CallType callType = call.getCallType();

		switch (callType) {
		case PROCEDURE_CALL:
			final ProcedureCall procedureCall = (ProcedureCall) call.unwrap();
			runProcedureCall(procedureCall, params);
			break;
		case SECTION_CALL:
			final SectionCall sectionCall = (SectionCall) call.unwrap();
			runSectionCall(sectionCall, params);
			break;
		default:
		}
	}

	protected void runProcedureCall(final ProcedureCall call, final CobolInterpreterParams params) {
		final Paragraph paragraph = call.getParagraph();
		paragraphHandler.run(paragraph, params);
	}

	protected void runSectionCall(final SectionCall call, final CobolInterpreterParams params) {
		final Section section = call.getSection();
		sectionHandler.run(section, params);
	}
}
