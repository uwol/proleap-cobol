package io.proleap.servlets;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@WebServlet(ServiceStatusServlet.PATTERN)
public class ServiceStatusServlet extends AbstractServlet {

	public static final String PATTERN = "/service_status";

	private static final long serialVersionUID = 1L;

	@Override
	protected void doGet(final HttpServletRequest req, final HttpServletResponse resp)
			throws ServletException, IOException {
		enableCors(req, resp);
		disableCache(resp);
	}
}
