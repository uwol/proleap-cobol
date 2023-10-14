package io.proleap.cobol.servlets.cron;

import java.io.IOException;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import io.proleap.cobol.context.ApplicationContextUtils;

@WebServlet(urlPatterns = WarmupCronServlet.PATTERN, loadOnStartup = 1)
public class WarmupCronServlet extends HttpServlet {

	public static final String PATTERN = "/cron/warmup";

	private static final long serialVersionUID = 1L;

	@Override
	public void service(final HttpServletRequest request, final HttpServletResponse response) throws IOException {
		ApplicationContextUtils.warmup();
	}
}