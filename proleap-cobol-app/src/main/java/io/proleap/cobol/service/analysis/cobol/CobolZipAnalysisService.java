package io.proleap.cobol.service.analysis.cobol;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public interface CobolZipAnalysisService {

	void analyze(HttpServletRequest req, HttpServletResponse res);
}
