package io.proleap.servlets.cobol.execute;

import static io.proleap.context.ApplicationContextUtils.getBean;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.proleap.service.execute.cobol.CobolTextExecuteService;
import io.proleap.servlets.AbstractServlet;

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
