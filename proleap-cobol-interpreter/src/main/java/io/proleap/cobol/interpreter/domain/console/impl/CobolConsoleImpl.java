package io.proleap.cobol.interpreter.domain.console.impl;

import java.util.ArrayList;
import java.util.List;

import io.proleap.cobol.interpreter.domain.console.CobolConsole;

public class CobolConsoleImpl implements CobolConsole {

	protected final List<String> lines = new ArrayList<>();

	protected boolean noAdvancing = false;

	@Override
	public void addLine(final String line, final boolean noAdvancing) {
		if (this.noAdvancing) {
			final int index = lines.size() - 1;
			final String lastLine = lines.get(index);
			lines.set(index, lastLine + line);
		} else {
			lines.add(line);
		}

		this.noAdvancing = noAdvancing;
	}

	@Override
	public void addLines(final List<String> lines) {
		this.lines.addAll(lines);

		noAdvancing = false;
	}

	@Override
	public List<String> getLines() {
		return lines;
	}

	@Override
	public void setNoAdvancing(final boolean noAdvancing) {
		this.noAdvancing = noAdvancing;
	}
}
