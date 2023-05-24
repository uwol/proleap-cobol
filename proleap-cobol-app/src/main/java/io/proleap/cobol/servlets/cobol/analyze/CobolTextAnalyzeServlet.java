package io.proleap.cobol.servlets.cobol.analyze;

import static io.proleap.cobol.context.ApplicationContextUtils.getBean;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.proleap.cobol.service.analysis.cobol.CobolTextAnalysisService;
import io.proleap.cobol.servlets.AbstractServlet;

@WebServlet(CobolTextAnalyzeServlet.PATTERN)
public class CobolTextAnalyzeServlet extends AbstractServlet {

	public static final String PATTERN = "/v1/cobol/analyze/text";

	private static final long serialVersionUID = 1L;

	@Override
	public void doPost(final HttpServletRequest req, final HttpServletResponse resp) {
		enableCors(req, resp);
		disableCache(resp);
		setContentTypeApplicationJson(resp);

		getBean(CobolTextAnalysisService.class).analyze(req, resp);
	}
}
