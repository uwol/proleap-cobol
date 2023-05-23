package io.proleap.cobol.api.environment.inputoutput.filecontrol;

import java.io.IOException;

public interface FileControlService {

	void close(FileControlEntry fileControlEntry) throws IOException;

	void openExtend(FileControlEntry fileControlEntry) throws IOException;

	void openInput(FileControlEntry fileControlEntry) throws IOException;

	void openInputOutput(FileControlEntry fileControlEntry) throws IOException;

	void openOutput(FileControlEntry fileControlEntry) throws IOException;

	void read(FileControlEntry fileControlEntry);

	void read(FileControlEntry fileControlEntry, Object object);

	void write(FileControlEntry fileControlEntry);
}
