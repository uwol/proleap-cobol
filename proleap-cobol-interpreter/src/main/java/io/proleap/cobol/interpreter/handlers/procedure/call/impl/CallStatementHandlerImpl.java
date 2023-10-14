package io.proleap.cobol.interpreter.handlers.procedure.call.impl;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.call.ByContent;
import io.proleap.cobol.asg.metamodel.procedure.call.ByContentPhrase;
import io.proleap.cobol.asg.metamodel.procedure.call.ByReference;
import io.proleap.cobol.asg.metamodel.procedure.call.ByReferencePhrase;
import io.proleap.cobol.asg.metamodel.procedure.call.ByValue;
import io.proleap.cobol.asg.metamodel.procedure.call.ByValuePhrase;
import io.proleap.cobol.asg.metamodel.procedure.call.CallStatement;
import io.proleap.cobol.asg.metamodel.procedure.call.GivingPhrase;
import io.proleap.cobol.asg.metamodel.procedure.call.UsingParameter;
import io.proleap.cobol.asg.metamodel.procedure.call.UsingParameter.ParameterType;
import io.proleap.cobol.asg.metamodel.procedure.call.UsingPhrase;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.domain.state.impl.CobolStateImpl;
import io.proleap.cobol.interpreter.exception.CobolInterpreterException;
import io.proleap.cobol.interpreter.handlers.procedure.call.CallStatementHandler;
import io.proleap.cobol.interpreter.handlers.procedure.call.cobolfiles.CobolWordCobolFileFinder;
import io.proleap.cobol.interpreter.handlers.procedure.impl.StatementHandlerImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.params.impl.CobolInterpreterParamsImpl;
import io.proleap.cobol.interpreter.runner.CobolInterpreterRunner;
import io.proleap.cobol.interpreter.service.address.CobolAddressGroupService;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;

@Singleton
public class CallStatementHandlerImpl extends StatementHandlerImpl<CallStatement> implements CallStatementHandler {

	@Inject
	private CobolAddressGroupService addressGroupService;

	@Inject
	private CobolInterpreterRunner cobolInterpreterRunner;

	@Inject
	private CobolWordCobolFileFinder cobolWordCobolFileFinder;

	@Inject
	private CobolDataDescriptionEntryService dataDescriptionEntryService;

	@Inject
	private CobolStorageService storageService;

	@Inject
	private CobolValueService valueService;

	@Inject
	private CobolValueStmtService valueStmtService;

	@Override
	@PostConstruct
	public void afterPropertiesSet() {
		statementsHandler.registerStatement(this);
	}

	protected CobolInterpreterParams createCallInterpreterParams(final UsingPhrase usingPhrase,
			final GivingPhrase givingPhrase, final CobolInterpreterParams params) {
		final CobolInterpreterParams result = new CobolInterpreterParamsImpl();

		result.setCobolFileExtensions(params.getCobolFileExtensions());
		result.setCobolFiles(params.getCobolFiles());
		result.setParserParams(params.getParserParams());
		result.setReferenceParams(new ArrayList<>());
		result.setValueParams(new ArrayList<>());

		final CobolStateImpl paramState = new CobolStateImpl();

		if (usingPhrase != null) {
			for (final UsingParameter usingParameter : usingPhrase.getUsingParameters()) {
				final ParameterType parameterType = usingParameter.getParameterType();

				switch (parameterType) {
				case VALUE:
					final ByValuePhrase byValuePhrase = usingParameter.getByValuePhrase();

					for (final ByValue byValue : byValuePhrase.getByValues()) {
						final CobolValue value = valueStmtService.getValue(byValue.getValueStmt(),
								params.getState().getStorage());
						result.getValueParams().add(addressGroupService.createAddressGroup(value, null));
					}
					break;
				case CONTENT:
					final ByContentPhrase byContentPhrase = usingParameter.getByContentPhrase();

					for (final ByContent byContent : byContentPhrase.getByContents()) {
						final CobolValue value = valueStmtService.getValue(byContent.getValueStmt(),
								params.getState().getStorage());
						result.getReferenceParams().add(addressGroupService.createAddressGroup(value, null));
					}
					break;
				case REFERENCE:
				default:
					final ByReferencePhrase byReferencePhrase = usingParameter.getByReferencePhrase();

					for (final ByReference byReference : byReferencePhrase.getByReferences()) {
						final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
								.getDataDescriptionEntry(byReference.getValueStmt());
						result.getReferenceParams().add(
								storageService.getAddressGroup(dataDescriptionEntry, params.getState().getStorage()));
					}
					break;
				}
			}
		}

		if (givingPhrase != null) {
			final DataDescriptionEntry dataDescriptionEntry = dataDescriptionEntryService
					.getDataDescriptionEntry(givingPhrase.getGivingCall());
			result.setGivingParam(storageService.getAddressGroup(dataDescriptionEntry, params.getState().getStorage()));
		}

		result.setState(paramState);

		return result;
	}

	@Override
	public StatementType getStatementType() {
		return StatementTypeEnum.CALL;
	}

	@Override
	public void run(final CallStatement statement, final CobolInterpreterParams params) {
		final CobolValue cobolValue = valueStmtService.getValue(statement.getProgramValueStmt(),
				params.getState().getStorage());
		final String cobolFileIdentier = valueService.getAsString(cobolValue, statement.getProgramUnit());
		final File inputFile = cobolWordCobolFileFinder.findCobolFile(params, cobolFileIdentier);

		if (inputFile == null) {
			throw new CobolInterpreterException("Could not find file for " + cobolFileIdentier);
		} else {
			try {
				final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, params.getParserParams());
				final ProgramUnit programUnit = program.getCompilationUnit().getProgramUnit();

				final CobolInterpreterParams callParams = createCallInterpreterParams(statement.getUsingPhrase(),
						statement.getGivingPhrase(), params);

				final CobolState resultState = cobolInterpreterRunner.run(programUnit, callParams);
				params.getState().getConsole().addLines(resultState.getConsole().getLines());
				params.getState().incOps(resultState.getOps());
			} catch (final IOException e) {
				throw new CobolInterpreterException(e);
			}
		}
	}
}
