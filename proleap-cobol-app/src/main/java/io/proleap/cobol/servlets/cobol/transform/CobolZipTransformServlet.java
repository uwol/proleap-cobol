package io.proleap.cobol.servlets.cobol.transform;

import static io.proleap.cobol.context.ApplicationContextUtils.getBean;

import javax.servlet.annotation.MultipartConfig;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.proleap.cobol.service.transform.cobol.CobolZipTransformService;
import io.proleap.cobol.servlets.AbstractServlet;

@MultipartConfig
@WebServlet(CobolZipTransformServlet.PATTERN)
public class CobolZipTransformServlet extends AbstractServlet {

	public static final String PATTERN = "/v1/cobol/transform/zip";

	private static final long serialVersionUID = 1L;

	private static final String TRANSFORMATION_ZIP_NAME = "proleap-cobol-artifact.zip";

	@Override
	public void doPost(final HttpServletRequest req, final HttpServletResponse resp) {
		enableCors(req, resp);
		disableCache(resp);

		resp.setHeader("Content-Disposition", "attachment; filename=" + TRANSFORMATION_ZIP_NAME);
		resp.setContentType("application/zip");

		getBean(CobolZipTransformService.class).transform(req, resp);
	}
}
