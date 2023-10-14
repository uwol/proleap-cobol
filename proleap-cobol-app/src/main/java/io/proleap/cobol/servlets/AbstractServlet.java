package io.proleap.cobol.servlets;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.stream.Collectors;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

public abstract class AbstractServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	protected void disableCache(final HttpServletResponse resp) {
		resp.setHeader("Cache-Control", "no-store, must-revalidate");
	}

	@Override
	protected void doOptions(final HttpServletRequest req, final HttpServletResponse resp)
			throws ServletException, IOException {
		enableCors(req, resp);

		resp.setStatus(HttpServletResponse.SC_OK);
	}

	protected void enableCache(final int maxAgeSeconds, final HttpServletResponse resp) {
		resp.setHeader("Cache-Control", "public, max-age=" + maxAgeSeconds);
	}

	protected void enableCors(final HttpServletRequest req, final HttpServletResponse resp) {
		resp.setHeader("Access-Control-Allow-Headers", req.getHeader("Access-Control-Request-Headers"));
		resp.setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
		resp.setHeader("Access-Control-Allow-Origin", "*");
	}

	protected ObjectMapper objectMapper() {
		final ObjectMapper result = new ObjectMapper();
		result.registerModule(new JavaTimeModule());
		return result;
	}

	protected String pathVariable(final HttpServletRequest req) {
		final String pathInfo = req.getPathInfo();
		final String result;

		if (pathInfo == null) {
			result = null;
		} else {
			final String[] parts = pathInfo.split("/");

			if (parts.length == 0) {
				result = null;
			} else {
				final int index = parts.length - 1;
				result = parts[index];
			}
		}

		return result;
	}

	protected String pathVariableFilename(final HttpServletRequest req) {
		final String pathVariable = pathVariable(req);
		final String result;

		if (pathVariable == null || pathVariable.isEmpty()) {
			result = pathVariable;
		} else if (pathVariable.indexOf(".") > 0) {
			result = pathVariable.substring(0, pathVariable.lastIndexOf("."));
		} else {
			result = pathVariable;
		}

		return result;
	}

	protected String readBody(final HttpServletRequest req) throws IOException {
		final InputStreamReader inputStreamReader = new InputStreamReader(req.getInputStream(), StandardCharsets.UTF_8);
		final BufferedReader bufferedReader = new BufferedReader(inputStreamReader);
		return bufferedReader.lines().collect(Collectors.joining(System.lineSeparator()));
	}

	protected <T> T readJson(final HttpServletRequest req, final Class<T> clazz) throws IOException {
		final String body = readBody(req);
		return objectMapper().readValue(body, clazz);
	}

	protected Map<String, String> readParams(final HttpServletRequest req) {
		return req.getParameterMap().entrySet().stream().collect(Collectors.toMap(entry -> {
			return entry.getKey();
		}, entry -> {
			return entry.getValue() == null ? null : entry.getValue()[0];
		}));
	}

	protected void setContentTypeApplicationJson(final HttpServletResponse resp) {
		resp.setContentType("application/json");
	}

	protected void setContentTypeApplicationPdf(final HttpServletResponse resp) {
		resp.setContentType("application/pdf");
	}

	protected void setContentTypeTextCsv(final HttpServletResponse resp) {
		resp.setContentType("text/csv");
	}

	protected void setContentTypeTextHtml(final HttpServletResponse resp) {
		resp.setContentType("text/html");
	}

	protected void setContentTypeTextPlain(final HttpServletResponse resp) {
		resp.setContentType("text/plain");
	}

	protected void writeBody(final String response, final HttpServletResponse resp) throws IOException {
		resp.getOutputStream().write(response.getBytes(StandardCharsets.UTF_8));
	}

	protected void writeJson(final Object obj, final HttpServletResponse resp) throws IOException {
		objectMapper().writeValue(resp.getOutputStream(), obj);
	}
}
