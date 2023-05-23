package io.proleap.service.output.impl;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.inject.Singleton;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.proleap.service.output.ZipOutputFileService;
import io.proleap.util.FilesUtils;

@Singleton
public class ZipOutputFileServiceImpl implements ZipOutputFileService {

	private final String CODE_VIEWS_ZIP_DIRECTORY = "codeviews";

	private final String ISSUES_ZIP_DIRECTORY = "issues";

	private final String LOGS_DIRECTORY = "logs";

	private final String SRC_MAIN_JAVA = "src/main/java";

	@Override
	public void addAnalysisCodeViewZipEntry(final File file, final long i, final ZipOutputStream zipOutputStream)
			throws IOException {
		final String directory = CODE_VIEWS_ZIP_DIRECTORY + "/" + i;
		addZipEntry(file, directory, zipOutputStream);
	}

	@Override
	public void addAnalysisIssueZipEntry(final Object issues, final ZipOutputStream zipOutputStream)
			throws IOException {
		final String filename = "issues.json";
		final String directory = ISSUES_ZIP_DIRECTORY;
		final String json = new ObjectMapper().writeValueAsString(issues);

		addZipEntry(json, filename, directory, zipOutputStream);
	}

	@Override
	public void addLogZipEntry(final String log, final String filename, final ZipOutputStream zipOutputStream)
			throws IOException {
		addZipEntry(log, filename, LOGS_DIRECTORY, zipOutputStream);
	}

	@Override
	public void addTransformationClassZipEntry(final File file, final ZipOutputStream zipOutputStream)
			throws IOException {
		addZipEntry(file, SRC_MAIN_JAVA, zipOutputStream);
	}

	@Override
	public void addZipEntry(final File file, final String zipDirectory, final ZipOutputStream zipOutputStream)
			throws IOException {
		final String filename = file.getName();
		final String zipDirectoryString = FilesUtils.getDirectoryString(zipDirectory);
		final ZipEntry ze = new ZipEntry(zipDirectoryString + filename);

		zipOutputStream.putNextEntry(ze);
		Files.copy(file.toPath(), zipOutputStream);
		zipOutputStream.closeEntry();
	}

	@Override
	public void addZipEntry(final String content, final String filename, final String zipDirectory,
			final ZipOutputStream zipOutputStream) throws IOException {
		final String zipDirectoryString = FilesUtils.getDirectoryString(zipDirectory);
		final ZipEntry ze = new ZipEntry(zipDirectoryString + filename);

		zipOutputStream.putNextEntry(ze);
		zipOutputStream.write(content.getBytes(StandardCharsets.UTF_8));
		zipOutputStream.closeEntry();
	}
}
