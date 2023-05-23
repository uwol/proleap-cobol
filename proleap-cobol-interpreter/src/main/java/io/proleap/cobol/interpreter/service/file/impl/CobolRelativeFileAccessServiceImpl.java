package io.proleap.cobol.interpreter.service.file.impl;

import javax.inject.Singleton;

import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.service.file.CobolRelativeFileAccessService;

@Singleton
public class CobolRelativeFileAccessServiceImpl implements CobolRelativeFileAccessService {

	@Override
	public void close(final CobolFile file) {
		file.setOpen(false);
	}

	@Override
	public void open(final CobolFile file) {
		file.setOpen(true);
	}
}
