package io.proleap.cobol.service.execute.cobol;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public interface CobolTextExecuteService {

	void execute(HttpServletRequest req, HttpServletResponse resp);
}
