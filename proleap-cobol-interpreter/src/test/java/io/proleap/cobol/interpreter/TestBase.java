package io.proleap.cobol.interpreter;

import java.io.File;
import java.util.Arrays;

import jakarta.inject.Inject;

import org.junit.jupiter.api.BeforeEach;

import io.micronaut.context.ApplicationContext;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.DataDivision;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.workingstorage.WorkingStorageSection;
import io.proleap.cobol.asg.metamodel.environment.EnvironmentDivision;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.InputOutputSection;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlParagraph;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.params.impl.CobolParserParamsImpl;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.interpreter.context.PostConstructBean;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.domain.state.impl.CobolStateImpl;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.params.impl.CobolInterpreterParamsImpl;
import io.proleap.cobol.interpreter.runner.CobolInterpreterRunner;
import io.proleap.cobol.interpreter.service.storage.CobolStorageService;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

public abstract class TestBase {

	@Inject
	private ApplicationContext applicationContext;

	@Inject
	protected CobolInterpreterRunner cobolInterpreterRunner;

	@Inject
	protected CobolStorageService storageService;

	@Inject
	protected CobolValueService valueService;

	protected CobolParserParams createDefaultParams() {
		return new CobolParserParamsImpl();
	}

	protected CobolInterpreterParams createInterpreterParams(final CobolParserParams parserParams) {
		final CobolInterpreterParams result = new CobolInterpreterParamsImpl();
		result.setParserParams(parserParams);
		result.setState(new CobolStateImpl());
		return result;
	}

	protected CobolParserParams createParserParams(final CobolSourceFormatEnum format, final File cobolFile) {
		final CobolParserParams result = createDefaultParams();
		result.setFormat(format);

		final File copyBooksDirectory = cobolFile.getParentFile();
		result.setCopyBookDirectories(Arrays.asList(copyBooksDirectory));

		return result;
	}

	protected FileControlEntry getFileControlEntry(final String key, final ProgramUnit programUnit) {
		final EnvironmentDivision environmentDivision = programUnit.getEnvironmentDivision();
		final InputOutputSection inputOutputSection = environmentDivision.getInputOutputSection();
		final FileControlParagraph fileControlParagraph = inputOutputSection.getFileControlParagraph();
		return fileControlParagraph.getFileControlEntry(key);
	}

	protected CobolValue getValue(final String key, final ProgramUnit programUnit, final CobolState state) {
		final DataDivision dataDivision = programUnit.getDataDivision();
		final WorkingStorageSection workingStorageSection = dataDivision.getWorkingStorageSection();
		final DataDescriptionEntry dataDescriptionEntry = workingStorageSection.getDataDescriptionEntry(key);
		final CobolValue result;

		if (dataDescriptionEntry == null) {
			result = null;
		} else {
			result = storageService.getValue(dataDescriptionEntry, state.getStorage());
		}

		return result;
	}

	@BeforeEach
	public void setUp() throws Exception {
		applicationContext.getBeansOfType(PostConstructBean.class);
	}
}
