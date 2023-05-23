package io.proleap.servlets.cobol.analyze;

import static io.proleap.context.ApplicationContextUtils.getBean;

import javax.servlet.annotation.MultipartConfig;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.proleap.service.analysis.cobol.CobolZipAnalysisService;
import io.proleap.servlets.AbstractServlet;

@MultipartConfig
@WebServlet(CobolZipAnalyzeServlet.PATTERN)
public class CobolZipAnalyzeServlet extends AbstractServlet {

	private static final String ANALYSIS_ZIP_NAME = "analysis.zip";

	public static final String PATTERN = "/v1/cobol/analyze/zip";

	private static final long serialVersionUID = 1L;

	@Override
	public void doPost(final HttpServletRequest req, final HttpServletResponse resp) {
		enableCors(req, resp);
		disableCache(resp);

		resp.setHeader("Content-Disposition", "attachment; filename=" + ANALYSIS_ZIP_NAME);
		resp.setContentType("application/zip");

		getBean(CobolZipAnalysisService.class).analyze(req, resp);
	}
}
