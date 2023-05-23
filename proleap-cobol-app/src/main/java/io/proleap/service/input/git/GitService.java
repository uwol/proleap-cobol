package io.proleap.service.input.git;

import java.io.File;
import java.io.IOException;

import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.api.errors.InvalidRemoteException;
import org.eclipse.jgit.api.errors.TransportException;

public interface GitService {

	File cloneIntoTempDir(String gitUri, String username, String password)
			throws InvalidRemoteException, TransportException, GitAPIException, IOException;
}
