package io.proleap.cobol.service.input;

import java.io.File;
import java.io.IOException;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;

import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.api.errors.InvalidRemoteException;
import org.eclipse.jgit.api.errors.TransportException;

public interface RequestCloneFilesService {

	File cloneFiles(HttpServletRequest req)
			throws IOException, ServletException, InvalidRemoteException, TransportException, GitAPIException;
}
