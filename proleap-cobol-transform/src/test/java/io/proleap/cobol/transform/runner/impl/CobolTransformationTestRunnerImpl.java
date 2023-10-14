package io.proleap.cobol.transform.runner.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.List;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.params.impl.CobolParserParamsImpl;
import io.proleap.cobol.asg.util.FilenameUtils;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.transform.java.runner.CobolTransformationRunner;
import io.proleap.cobol.transform.runner.CobolTransformationTestRunner;

@Singleton
public class CobolTransformationTestRunnerImpl implements CobolTransformationTestRunner {

	public final static String ARTIFACT_SUFFIX = ".java";

	private static final String JAVA_PACKAGE = "";

	private final static Logger LOG = LoggerFactory.getLogger(CobolTransformationTestRunnerImpl.class);

	@Inject
	private CobolTransformationRunner cobolTransformationRunner;

	protected void doCompareArtifact(final File outputFile, final File inputFile) throws IOException {
		final String outputFileData = Files.readString(outputFile.toPath(), StandardCharsets.UTF_8);
		final File inputFileDirectory = inputFile.getParentFile();
		final String baseName = FilenameUtils.getBaseName(inputFile.getName());
		final File referenceOutputFile = new File(
				inputFileDirectory.getAbsolutePath() + "/" + baseName + ARTIFACT_SUFFIX);
		final String referenceOutputFileData = Files.readString(referenceOutputFile.toPath(), StandardCharsets.UTF_8);

		if (referenceOutputFileData != null && !referenceOutputFileData.isEmpty()) {
			LOG.info("Comparing artifact file with file {}.", referenceOutputFile.getName());

			assertEquals(referenceOutputFileData, outputFileData);
		} else {
			LOG.info("Ignoring empty artifact file {}.", referenceOutputFile.getName());
		}
	}

	protected List<File> getCopyFiles(final File libDirectory) {
		return Arrays.asList(libDirectory.listFiles());
	}

	@Override
	public void transformFile(final File inputFile, final CobolSourceFormatEnum format) throws IOException {
		LOG.info("Transforming file {}.", inputFile.getName());

		final CobolParserParams params = new CobolParserParamsImpl();
		params.setFormat(format);

		final List<File> outputFiles = cobolTransformationRunner.transformFile(inputFile, JAVA_PACKAGE, params);
		doCompareArtifact(outputFiles.get(0), inputFile);
	}
}
