package io.proleap.api.service.input;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.File;

import org.junit.jupiter.api.Test;

import io.proleap.service.input.git.GitService;
import io.proleap.service.input.git.impl.GitServiceImpl;
import io.proleap.util.FilesUtils;

public class GitServiceIT {

	private static final String GIT_URL = "https://github.com/proleap/proleap-cobol-example.git";

	protected GitService gitService = new GitServiceImpl();

	@Test
	public void testCloneIntoTempDir() throws Exception {

		final File tempDir = gitService.cloneIntoTempDir(GIT_URL, null, null);

		assertNotNull(tempDir.listFiles());
		assertFalse(tempDir.listFiles().length == 0);

		FilesUtils.deleteFile(tempDir);
	}
}
