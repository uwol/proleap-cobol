package io.proleap.cobol.service.transform.cobol;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public interface CobolZipTransformService {

	void transform(HttpServletRequest req, HttpServletResponse resp);
}
