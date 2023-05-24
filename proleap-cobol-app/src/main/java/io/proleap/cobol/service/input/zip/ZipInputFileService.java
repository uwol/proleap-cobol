package io.proleap.cobol.service.input.zip;

import java.io.File;
import java.io.IOException;
import java.util.zip.ZipException;

public interface ZipInputFileService {

	File cloneIntoTempDir(File zip) throws ZipException, IOException;
}
