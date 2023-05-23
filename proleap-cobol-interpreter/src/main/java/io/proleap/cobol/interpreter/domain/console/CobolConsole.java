package io.proleap.cobol.interpreter.domain.console;

import java.util.List;

public interface CobolConsole {

	void addLine(String line, boolean noAdvancing);

	void addLines(List<String> lines);

	List<String> getLines();

	void setNoAdvancing(boolean noAdvancing);
}
