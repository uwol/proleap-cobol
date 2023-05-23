package io.proleap.cobol.interpreter.domain.file;

public interface CobolFile {

	enum FileType {
		INDEXED, RELATIVE, SEQUENTIAL
	}

	FileType getType();

	boolean isOpen();

	void setOpen(boolean open);
}
