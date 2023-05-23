package io.proleap.cobol.transform.java.runner;

import java.io.File;
import java.io.IOException;
import java.util.List;

import io.proleap.cobol.asg.params.CobolParserParams;

public interface CobolTransformationRunner {

	List<File> transformCode(String cobolCode, String compilationUnitName, String packageName, CobolParserParams params)
			throws IOException;

	List<File> transformFile(File cobolFile, String packageName, CobolParserParams params) throws IOException;
}
