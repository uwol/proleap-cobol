package io.proleap.util;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.Part;

import com.google.common.base.Strings;
import com.google.common.io.BaseEncoding;

public class RequestUtils {

	private static final String ANALYSIS_ID = "analysis_id";

	private static final String AUTH_TOKEN = "auth_token";

	private static final String GIT_PASSWORD = "git_password";

	private static final String GIT_URI = "git_uri";

	private static final String GIT_USERNAME = "git_username";

	private static final String PROJECT_ID = "project_id";

	private static final String TRANSFORMATION_ID = "transformation_id";

	public static String fetchAnalysisId(final HttpServletRequest req) throws IOException, ServletException {
		return fetchString(ANALYSIS_ID, req);
	}

	public static String fetchAuthToken(final HttpServletRequest req) throws IOException, ServletException {
		return fetchString(AUTH_TOKEN, req);
	}

	public static File fetchFile(final String name, final HttpServletRequest req) throws IOException, ServletException {
		File result = null;

		for (final Part part : req.getParts()) {
			if (name.equals(part.getName()) && !Strings.isNullOrEmpty(part.getSubmittedFileName())) {
				result = FilesUtils.createTempFile(part.getSubmittedFileName());
				Files.copy(part.getInputStream(), result.toPath());
				break;
			}
		}

		return result;
	}

	public static String fetchGitPassword(final HttpServletRequest req) throws IOException, ServletException {
		return fetchString(GIT_PASSWORD, req);
	}

	public static String fetchGitUri(final HttpServletRequest req) throws IOException, ServletException {
		return fetchString(GIT_URI, req);
	}

	public static String fetchGitUsername(final HttpServletRequest req) throws IOException, ServletException {
		return fetchString(GIT_USERNAME, req);
	}

	public static byte[] fetchHexBytes(final String name, final HttpServletRequest req)
			throws IOException, ServletException {
		final String hexString = fetchString(name, req);
		final byte[] result;

		if (Strings.isNullOrEmpty(hexString)) {
			result = null;
		} else {
			result = BaseEncoding.base16().decode(hexString.trim());
		}

		return result;
	}

	public static Long fetchLong(final String name, final HttpServletRequest req) throws IOException, ServletException {
		final Part part = req.getPart(name);
		final Long result;

		if (part == null) {
			result = null;
		} else {
			result = Long.valueOf(new String(part.getInputStream().readAllBytes(), StandardCharsets.UTF_8));
		}

		return result;
	}

	public static String fetchProjectId(final HttpServletRequest req) throws IOException, ServletException {
		return fetchString(PROJECT_ID, req);
	}

	public static String fetchString(final String name, final HttpServletRequest req)
			throws IOException, ServletException {
		final Part part = req.getPart(name);
		final String result;

		if (part == null) {
			result = null;
		} else {
			result = new String(part.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
		}

		return result;
	}

	public static String fetchTransformationId(final HttpServletRequest req) throws IOException, ServletException {
		return fetchString(TRANSFORMATION_ID, req);
	}
}
