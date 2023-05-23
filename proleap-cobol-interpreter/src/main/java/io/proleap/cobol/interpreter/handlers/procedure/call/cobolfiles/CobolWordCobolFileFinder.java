package io.proleap.cobol.interpreter.handlers.procedure.call.cobolfiles;

import java.io.File;

import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public interface CobolWordCobolFileFinder {

	File findCobolFile(CobolInterpreterParams params, String cobolFileIdentifier);
}
