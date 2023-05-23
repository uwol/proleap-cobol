package io.proleap.util;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;

import com.google.common.base.Strings;

public class FilesUtils {

	public static File createTempFile(final String name) throws IOException {
		final File result = new File(Files.createTempDirectory("tmp").toFile().getAbsolutePath(), name);
		return result;
	}

	public static void deleteFile(final File file) {
		if (file != null) {
			file.delete();
		}
	}

	public static void deleteFiles(final List<File> files) {
		if (files != null) {
			for (final File file : files) {
				file.delete();
			}
		}
	}

	public static String getDirectoryString(final String directory) {
		return !Strings.isNullOrEmpty(directory) ? directory + "/" : "";
	}
}
