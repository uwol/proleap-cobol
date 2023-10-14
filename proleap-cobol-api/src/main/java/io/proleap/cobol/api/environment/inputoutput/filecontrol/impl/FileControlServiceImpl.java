package io.proleap.cobol.api.environment.inputoutput.filecontrol.impl;

import java.io.IOException;

import jakarta.validation.Validation;
import jakarta.validation.Validator;

import io.proleap.cobol.api.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.api.environment.inputoutput.filecontrol.FileControlService;

public class FileControlServiceImpl implements FileControlService {

	@Override
	public void close(final FileControlEntry fileControlEntry) throws IOException {
		fileControlEntry.close();
	}

	@Override
	public void openExtend(final FileControlEntry fileControlEntry) throws IOException {
		fileControlEntry.openWrite(true);
	}

	@Override
	public void openInput(final FileControlEntry fileControlEntry) throws IOException {
		fileControlEntry.openRead();
	}

	@Override
	public void openInputOutput(final FileControlEntry fileControlEntry) throws IOException {
		fileControlEntry.openRead();
		fileControlEntry.openWrite(false);
	}

	@Override
	public void openOutput(final FileControlEntry fileControlEntry) throws IOException {
		fileControlEntry.openWrite(false);
	}

	@Override
	public void read(final FileControlEntry fileControlEntry) {

	}

	@Override
	public void read(final FileControlEntry fileControlEntry, final Object object) {

	}

	@Override
	public void write(final FileControlEntry fileControlEntry) {
		final Validator validator = Validation.buildDefaultValidatorFactory().getValidator();
		validator.validate(fileControlEntry);
	}
}
