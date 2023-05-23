package io.proleap.service.input.impl;

import java.io.File;
import java.io.IOException;

import javax.inject.Singleton;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;

import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.api.errors.InvalidRemoteException;
import org.eclipse.jgit.api.errors.TransportException;

import com.google.common.base.Strings;

import io.proleap.service.input.RequestCloneFilesService;
import io.proleap.service.input.git.GitService;
import io.proleap.service.input.git.impl.GitServiceImpl;
import io.proleap.service.input.zip.ZipInputFileService;
import io.proleap.service.input.zip.impl.ZipInputFileServiceImpl;
import io.proleap.util.RequestUtils;

@Singleton
public class RequestCloneFilesServiceImpl implements RequestCloneFilesService {

	private static final String ZIP_FILE = "zip_file";

	protected final GitService gitService = new GitServiceImpl();

	protected final ZipInputFileService zipFileService = new ZipInputFileServiceImpl();

	@Override
	public File cloneFiles(final HttpServletRequest req)
			throws IOException, ServletException, InvalidRemoteException, TransportException, GitAPIException {
		final String gitUri = RequestUtils.fetchGitUri(req);
		final String gitUsername = RequestUtils.fetchGitUsername(req);
		final String gitPassword = RequestUtils.fetchGitPassword(req);

		final File zipFile = RequestUtils.fetchFile(ZIP_FILE, req);
		final File result;

		if (!Strings.isNullOrEmpty(gitUri)) {
			result = gitService.cloneIntoTempDir(gitUri, gitUsername, gitPassword);
		} else if (zipFile != null) {
			result = zipFileService.cloneIntoTempDir(zipFile);
		} else {
			result = null;
		}

		return result;
	}
}
