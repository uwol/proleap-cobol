package io.proleap.cobol.servlets.cobol.analyze;

import static io.proleap.cobol.context.ApplicationContextUtils.getBean;

import jakarta.servlet.annotation.MultipartConfig;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import io.proleap.cobol.service.analysis.cobol.CobolZipAnalysisService;
import io.proleap.cobol.servlets.AbstractServlet;

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
