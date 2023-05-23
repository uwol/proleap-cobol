package io.proleap.cobol.transform.runner;

import java.io.File;
import java.io.IOException;

import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

/**
 * Cobol transform runner for JUnit tests.
 */
public interface CobolTransformationTestRunner {

	void transformFile(File inputFile, CobolSourceFormatEnum format) throws IOException;
}
