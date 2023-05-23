package io.proleap.service.execute.cobol;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public interface CobolTextExecuteService {

	void execute(HttpServletRequest req, HttpServletResponse resp);
}
