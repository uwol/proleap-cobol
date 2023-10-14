package io.proleap.cobol.servlets;

import java.io.IOException;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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
