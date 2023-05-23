package io.proleap.cobol.interpreter.domain.file.impl;

import io.proleap.cobol.interpreter.domain.file.CobolRelativeFile;

public class CobolRelativeFileImpl implements CobolRelativeFile {

	private boolean open;

	@Override
	public FileType getType() {
		return FileType.RELATIVE;
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
