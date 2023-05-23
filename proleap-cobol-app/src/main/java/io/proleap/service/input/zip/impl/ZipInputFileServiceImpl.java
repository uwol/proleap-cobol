package io.proleap.service.input.zip.impl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.proleap.service.input.zip.ZipInputFileService;

public class ZipInputFileServiceImpl implements ZipInputFileService {

	private final static Logger LOG = LoggerFactory.getLogger(ZipInputFileServiceImpl.class);

	@Override
	public File cloneIntoTempDir(final File zip) throws ZipException, IOException {
		LOG.info("Uncompressing zip file");

		final File result = Files.createTempDirectory("tmp").toFile();
		final ZipFile zipFile = new ZipFile(zip);
		final Enumeration<? extends ZipEntry> entries = zipFile.entries();

		try {
			while (entries.hasMoreElements()) {
				final ZipEntry entry = entries.nextElement();
				copyEntryToFile(result, entry, zipFile);
			}
		} finally {
			zipFile.close();
		}

		LOG.info("Uncompressed zip file");

		return result;
	}

	protected void copyEntryToFile(final File targetTempDir, final ZipEntry entry, final ZipFile zipFile) {
		final File targetTempFile = new File(targetTempDir.getAbsolutePath() + File.separator + entry.getName());

		if (entry.isDirectory()) {
			targetTempFile.mkdirs();
		} else {
			try {
				targetTempFile.getParentFile().mkdirs();
				targetTempFile.createNewFile();

				Files.copy(zipFile.getInputStream(entry), targetTempFile.toPath());
			} catch (final IOException e) {
				LOG.warn(e.getMessage(), e);
			} finally {
			}
		}
	}
}
