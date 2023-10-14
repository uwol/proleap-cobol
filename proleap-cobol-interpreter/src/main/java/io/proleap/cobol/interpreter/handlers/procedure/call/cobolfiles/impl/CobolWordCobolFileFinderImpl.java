package io.proleap.cobol.interpreter.handlers.procedure.call.cobolfiles.impl;

import java.io.File;

import jakarta.inject.Singleton;

import io.proleap.cobol.asg.util.FilenameUtils;
import io.proleap.cobol.interpreter.handlers.procedure.call.cobolfiles.CobolWordCobolFileFinder;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

@Singleton
public class CobolWordCobolFileFinderImpl implements CobolWordCobolFileFinder {

	@Override
	public File findCobolFile(final CobolInterpreterParams params, final String cobolFileIdentifier) {
		if (params.getCobolFiles() != null) {
			for (final File cobolFile : params.getCobolFiles()) {
				if (isMatchingCobolFile(cobolFile, params, cobolFileIdentifier)) {
					return cobolFile;
				}
			}
		}

		return null;
	}

	protected boolean isMatchingCobolFile(final File cobolFileCandidate, final CobolInterpreterParams params,
			final String cobolFileIdentifier) {
		if (params.getCobolFileExtensions() != null) {
			for (final String cobolFileExtension : params.getCobolFileExtensions()) {
				if (isMatchingCobolFileWithExtension(cobolFileCandidate, cobolFileIdentifier, cobolFileExtension)) {
					return true;
				}
			}

			return false;
		} else {
			return isMatchingCobolFileWithoutExtension(cobolFileCandidate, cobolFileIdentifier);
		}
	}

	protected boolean isMatchingCobolFileWithExtension(final File cobolFileCandidate, final String cobolFileIdentifier,
			final String cobolFileExtension) {
		final String cobolFileFilename = cobolFileExtension == null || cobolFileExtension.isEmpty()
				? cobolFileIdentifier
				: cobolFileIdentifier + "." + cobolFileExtension;
		final String cobolFileCandidateName = cobolFileCandidate.getName();
		final boolean result = cobolFileFilename.equalsIgnoreCase(cobolFileCandidateName);
		return result;
	}

	protected boolean isMatchingCobolFileWithoutExtension(final File cobolFileCandidate,
			final String cobolFileIdentifier) {
		final String cobolFileCandidateBaseName = FilenameUtils.getBaseName(cobolFileCandidate.getName());
		final boolean result = cobolFileCandidateBaseName.equalsIgnoreCase(cobolFileIdentifier);
		return result;
	}
}
