package io.proleap.cobol.service.input.git.impl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;

import org.eclipse.jgit.api.CloneCommand;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.api.errors.InvalidRemoteException;
import org.eclipse.jgit.api.errors.TransportException;
import org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Strings;

import io.proleap.cobol.service.input.git.BonnaFideTrustManager;
import io.proleap.cobol.service.input.git.GitService;

public class GitServiceImpl implements GitService {

	private final static Logger LOG = LoggerFactory.getLogger(GitServiceImpl.class);

	public GitServiceImpl() {
		trustAllCertificates();
	}

	@Override
	public File cloneIntoTempDir(final String gitUri, final String username, final String password)
			throws InvalidRemoteException, TransportException, GitAPIException, IOException {
		LOG.info("Cloning git repository");

		final File result = Files.createTempDirectory("tmp").toFile();
		final CloneCommand cloneCommand = Git.cloneRepository().setURI(gitUri).setDirectory(result);

		if (!Strings.isNullOrEmpty(username) && !Strings.isNullOrEmpty(password)) {
			cloneCommand.setCredentialsProvider(new UsernamePasswordCredentialsProvider(username, password));
		}

		cloneCommand.call();

		LOG.info("Cloned git repository");

		return result;
	}

	protected void trustAllCertificates() {
		try {
			final SSLContext sc = SSLContext.getInstance("SSL");
			sc.init(null, new TrustManager[] { new BonnaFideTrustManager() }, null);
			HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
		} catch (NoSuchAlgorithmException | KeyManagementException e) {
		}
	}
}
