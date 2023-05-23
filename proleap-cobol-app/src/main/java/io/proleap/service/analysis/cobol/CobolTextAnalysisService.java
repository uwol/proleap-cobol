package io.proleap.service.analysis.cobol;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public interface CobolTextAnalysisService {

	void analyze(HttpServletRequest req, HttpServletResponse resp);
}
