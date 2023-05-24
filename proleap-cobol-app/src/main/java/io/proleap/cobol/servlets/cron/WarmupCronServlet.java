package io.proleap.cobol.servlets.cron;

import java.io.IOException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

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