package io.proleap.cobol.interpreter.service.file;

import io.proleap.cobol.interpreter.domain.file.CobolFile;

public interface CobolIndexedFileAccessService {

	void close(CobolFile file);

	void open(CobolFile file);
}
