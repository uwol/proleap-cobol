package io.proleap.cobol.filters;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.nio.charset.StandardCharsets;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.annotation.WebFilter;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import com.fasterxml.jackson.databind.JsonMappingException;

import io.proleap.cobol.asg.exception.CobolParserException;
import io.proleap.cobol.preprocessor.exception.CobolPreprocessorException;

@WebFilter(ErrorHandlerFilter.PATTERN)
public class ErrorHandlerFilter implements Filter {

	public static final String PATTERN = "/*";

	@Override
	public void destroy() {
	}

	@Override
	public void doFilter(final ServletRequest request, final ServletResponse response, final FilterChain filterChain)
			throws IOException, ServletException {
		try {
			filterChain.doFilter(request, response);
		} catch (final JsonMappingException e) {
			enableCors(request, response);
			/*
			 * JSON mapping errors can happen often, in case of bad requests or changes in
			 * the widget -> return bad request
			 */
			setStatusBadRequest(request, response);
		} catch (final CobolParserException | CobolPreprocessorException  e) {
			enableCors(request, response);
			setStatusBadRequest(request, response);
			response.getOutputStream().write(e.getMessage().getBytes(StandardCharsets.UTF_8));
		}
	}

	private void enableCors(final HttpServletRequest req, final HttpServletResponse resp) {
		resp.setHeader("Access-Control-Allow-Headers", req.getHeader("Access-Control-Request-Headers"));
		resp.setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
		resp.setHeader("Access-Control-Allow-Origin", "*");
	}

	private void enableCors(final ServletRequest request, final ServletResponse response) {
		if (request instanceof HttpServletRequest && response instanceof HttpServletResponse) {
			final HttpServletRequest httpServletRequest = (HttpServletRequest) request;
			final HttpServletResponse httpServletResponse = (HttpServletResponse) response;

			enableCors(httpServletRequest, httpServletResponse);
		}
	}

	@Override
	public void init(final FilterConfig filterConfig) throws ServletException {
	}

	private void setStatusBadRequest(final ServletRequest request, final ServletResponse response) {
		if (response instanceof HttpServletResponse) {
			final HttpServletResponse httpServletResponse = (HttpServletResponse) response;
			httpServletResponse.setStatus(HttpURLConnection.HTTP_BAD_REQUEST);
		}
	}
}
