package io.proleap.cobol.service.execute.cobol.impl;

import java.util.Collections;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.runner.CobolInterpreterRunner;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.service.execute.cobol.CobolTextExecuteService;
import io.proleap.cobol.service.execute.cobol.dto.CobolExecutionParamDto;
import io.proleap.cobol.service.execute.cobol.dto.CobolExecutionResultDto;
import io.proleap.cobol.service.execute.cobol.dto.ExecutionTupleDto;
import io.proleap.cobol.service.impl.AbstractCobolService;

@Singleton
public class CobolTextExecuteServiceImpl extends AbstractCobolService implements CobolTextExecuteService {

	@Inject
	private CobolInterpreterRunner cobolInterpreterRunner;

	@Inject
	private CobolStorageService storageService;

	@Override
	public void execute(final HttpServletRequest req, final HttpServletResponse resp) {
		final CobolExecutionResultDto result = new CobolExecutionResultDto();

		try {
			final CobolExecutionParamDto param = new ObjectMapper().readValue(req.getInputStream(),
					CobolExecutionParamDto.class);

			final CobolSourceFormatEnum format = CobolSourceFormatEnum.valueOf(param.format);

			final CobolParserParams parserParams = createParserParams();
			parserParams.setFormat(format);

			final Program program = new CobolParserRunnerImpl().analyzeCode(param.code, "example", parserParams);
			final CompilationUnit compilationUnit = program.getCompilationUnits().get(0);
			final ProgramUnit programUnit = compilationUnit.getProgramUnit();

			{
				final CobolInterpreterParams interpreterParams = createInterpreterParams(parserParams);
				final CobolState state = cobolInterpreterRunner.run(programUnit, interpreterParams);

				result.output = state.getConsole().getLines();

				for (final DataDescriptionEntry dataDescriptionEntry : state.getStorage().getKeys()) {
					final String name = dataDescriptionEntry.getName();

					if (name == null) {
					} else {
						final ExecutionTupleDto tuple = new ExecutionTupleDto();
						tuple.key = name;
						tuple.value = String.valueOf(storageService.getValue(dataDescriptionEntry, state.getStorage()));
						result.values.add(tuple);
					}
				}
			}

			Collections.sort(result.values);

			new ObjectMapper().writeValue(resp.getOutputStream(), result);
		} catch (final Exception e) {
			handleException(resp, e);
		}
	}
}
