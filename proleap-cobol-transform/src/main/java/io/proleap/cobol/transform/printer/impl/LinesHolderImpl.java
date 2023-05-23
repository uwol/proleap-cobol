package io.proleap.cobol.transform.printer.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.proleap.cobol.transform.printer.LinesHolder;

public class LinesHolderImpl implements LinesHolder {

	protected Map<Integer, String> linesMap;

	public LinesHolderImpl(final List<String> lines) {
		linesMap = new HashMap<Integer, String>();

		for (int i = 0; i < lines.size(); i++) {
			final String line = lines.get(i);
			linesMap.put(i, line);
		}
	}

	@Override
	public String getLine(final Integer lineNumber) {
		final String line = linesMap.get(lineNumber);
		final String result;

		if (line == null || line.isEmpty()) {
			result = "";
		} else {
			result = line;
		}

		linesMap.remove(lineNumber);
		return result;
	}
}
