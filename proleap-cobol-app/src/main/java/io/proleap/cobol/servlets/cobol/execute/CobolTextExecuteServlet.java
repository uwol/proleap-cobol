package io.proleap.cobol.servlets.cobol.execute;

import static io.proleap.cobol.context.ApplicationContextUtils.getBean;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import io.proleap.cobol.service.execute.cobol.CobolTextExecuteService;
import io.proleap.cobol.servlets.AbstractServlet;

@WebServlet(CobolTextExecuteServlet.PATTERN)
public class CobolTextExecuteServlet extends AbstractServlet {

	public static final String PATTERN = "/v1/cobol/execute/text";

	private static final long serialVersionUID = 1L;

	@Override
	public void doPost(final HttpServletRequest req, final HttpServletResponse resp) {
		enableCors(req, resp);
		disableCache(resp);
		setContentTypeApplicationJson(resp);

		getBean(CobolTextExecuteService.class).execute(req, resp);
	}
}
