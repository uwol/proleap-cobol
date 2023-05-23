package io.proleap.cobol.interpreter.domain.file;

import java.util.List;

public interface CobolSequentialFile extends CobolFile {

	static final int INITIAL_INDEX = -1;

	int getIndex();

	List<String> getRecords();

	boolean hasNext();

	String next();

	void setIndex(int index);
}
