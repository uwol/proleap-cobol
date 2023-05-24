package io.proleap.cobol.service.analysis.cobol;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public interface CobolZipAnalysisService {

	void analyze(HttpServletRequest req, HttpServletResponse res);
}
