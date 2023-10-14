package io.proleap.cobol.servlets.cobol.transform;

import static io.proleap.cobol.context.ApplicationContextUtils.getBean;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import io.proleap.cobol.service.transform.cobol.CobolTextTransformService;
import io.proleap.cobol.servlets.AbstractServlet;

@WebServlet(CobolTextTransformServlet.PATTERN)
public class CobolTextTransformServlet extends AbstractServlet {

	public static final String PATTERN = "/v1/cobol/transform/text";

	private static final long serialVersionUID = 1L;

	@Override
	public void doPost(final HttpServletRequest req, final HttpServletResponse resp) {
		enableCors(req, resp);
		disableCache(resp);
		setContentTypeTextPlain(resp);

		getBean(CobolTextTransformService.class).transform(req, resp);
	}
}
