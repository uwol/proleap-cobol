package io.proleap.cobol.interpreter.domain.file.impl;

import io.proleap.cobol.interpreter.domain.file.CobolIndexedFile;

public class CobolIndexedFileImpl implements CobolIndexedFile {

	private boolean open;

	@Override
	public FileType getType() {
		return FileType.INDEXED;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void setOpen(final boolean open) {
		this.open = open;
	}
}
