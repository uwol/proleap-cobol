package io.proleap.service.transform.cobol;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public interface CobolZipTransformService {

	void transform(HttpServletRequest req, HttpServletResponse resp);
}
