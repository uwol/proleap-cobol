package io.proleap.service.output;

import java.io.File;
import java.io.IOException;
import java.util.zip.ZipOutputStream;

public interface ZipOutputFileService {

	void addAnalysisCodeViewZipEntry(File file, long i, ZipOutputStream zipOutputStream) throws IOException;

	void addAnalysisIssueZipEntry(Object issues, ZipOutputStream zipOutputStream) throws IOException;

	void addLogZipEntry(String content, String filename, ZipOutputStream zipOutputStream) throws IOException;

	void addTransformationClassZipEntry(File file, ZipOutputStream zipOutputStream) throws IOException;

	void addZipEntry(File file, String zipDirectory, ZipOutputStream zipOutputStream) throws IOException;

	void addZipEntry(String content, String filename, String zipDirectory, ZipOutputStream zipOutputStream)
			throws IOException;
}
