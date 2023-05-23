package io.proleap.cobol.interpreter.domain.file.impl;

import java.util.ArrayList;
import java.util.List;

import io.proleap.cobol.interpreter.domain.file.CobolSequentialFile;

public class CobolSequentialFileImpl implements CobolSequentialFile {

	private int index = INITIAL_INDEX;

	private boolean open;

	private final List<String> records = new ArrayList<>();

	@Override
	public int getIndex() {
		return index;
	}

	@Override
	public List<String> getRecords() {
		return records;
	}

	@Override
	public FileType getType() {
		return FileType.SEQUENTIAL;
	}

	@Override
	public boolean hasNext() {
		return index < records.size() - 1;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public String next() {
		index++;

		if (index < records.size()) {
			return records.get(index);
		}

		return null;
	}

	@Override
	public void setIndex(final int index) {
		this.index = index;
	}

	@Override
	public void setOpen(final boolean open) {
		this.open = open;
	}
}
