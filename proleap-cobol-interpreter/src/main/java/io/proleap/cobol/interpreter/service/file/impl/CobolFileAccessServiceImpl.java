package io.proleap.cobol.interpreter.service.file.impl;

import java.util.List;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.AccessModeClause;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.OrganizationClause;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.OrganizationClause.Mode;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.domain.file.CobolSequentialFile;
import io.proleap.cobol.interpreter.domain.file.impl.CobolIndexedFileImpl;
import io.proleap.cobol.interpreter.domain.file.impl.CobolRelativeFileImpl;
import io.proleap.cobol.interpreter.domain.file.impl.CobolSequentialFileImpl;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.service.file.CobolFileAccessService;
import io.proleap.cobol.interpreter.service.file.CobolIndexedFileAccessService;
import io.proleap.cobol.interpreter.service.file.CobolRelativeFileAccessService;
import io.proleap.cobol.interpreter.service.file.CobolSequentialFileAccessService;
import io.proleap.cobol.interpreter.service.file.domain.KeyCriterion;
import io.proleap.cobol.interpreter.service.file.domain.SortCriterion;

@Singleton
public class CobolFileAccessServiceImpl implements CobolFileAccessService {

	@Inject
	private CobolIndexedFileAccessService indexedFileAccessService;

	@Inject
	private CobolRelativeFileAccessService relativeFileAccessService;

	@Inject
	private CobolSequentialFileAccessService sequentialFileAccessService;

	@Override
	public CobolFile assureFile(final FileControlEntry fileControlEntry, final CobolState state) {
		if (state.getFile(fileControlEntry) == null) {
			final OrganizationClause organizationClause = fileControlEntry.getOrganizationClause();
			final Mode mode = organizationClause == null ? null : organizationClause.getMode();
			final CobolFile file;

			if (mode == null) {
				file = new CobolSequentialFileImpl();
			} else {
				switch (mode) {
				case INDEXED:
					file = new CobolIndexedFileImpl();
					break;
				case RELATIVE:
					file = new CobolRelativeFileImpl();
					break;
				default:
				case SEQUENTIAL:
					file = new CobolSequentialFileImpl();
					break;
				}
			}

			state.putFile(fileControlEntry, file);
		}

		return state.getFile(fileControlEntry);
	}

	@Override
	public void close(final CobolFile file, final FileControlEntry fileControlEntry) {
		switch (file.getType()) {
		case INDEXED:
			indexedFileAccessService.close(file);
			break;
		case RELATIVE:
			relativeFileAccessService.close(file);
			break;
		case SEQUENTIAL:
		default:
			sequentialFileAccessService.close((CobolSequentialFile) file);
			break;
		}
	}

	@Override
	public void copy(final CobolFile file1, final CobolFile file2, final FileControlEntry fileControlEntry) {
		switch (file1.getType()) {
		case INDEXED:
			break;
		case RELATIVE:
			break;
		case SEQUENTIAL:
		default:
			sequentialFileAccessService.copy((CobolSequentialFile) file1, (CobolSequentialFile) file2);
			break;
		}
	}

	@Override
	public void delete(final CobolFile file, final List<CobolAddress> addresses, final KeyCriterion criterion,
			final FileControlEntry fileControlEntry, final ProgramUnit programUnit) {
		switch (file.getType()) {
		case INDEXED:
			break;
		case RELATIVE:
			break;
		case SEQUENTIAL:
		default:
			sequentialFileAccessService.delete((CobolSequentialFile) file, addresses, criterion, programUnit);
			break;
		}
	}

	protected AccessModeClause.Mode determineMode(final FileControlEntry fileControlEntry) {
		final AccessModeClause accessModeClause = fileControlEntry.getAccessModeClause();
		return accessModeClause == null ? AccessModeClause.Mode.SEQUENTIAL : accessModeClause.getMode();
	}

	@Override
	public void open(final CobolFile file, final FileControlEntry fileControlEntry) {
		switch (file.getType()) {
		case INDEXED:
			indexedFileAccessService.open(file);
			break;
		case RELATIVE:
			relativeFileAccessService.open(file);
			break;
		case SEQUENTIAL:
		default:
			sequentialFileAccessService.open((CobolSequentialFile) file);
			break;
		}
	}

	@Override
	public String read(final CobolFile file, final List<CobolAddress> addresses, final KeyCriterion criterion,
			final FileControlEntry fileControlEntry, final ProgramUnit programUnit) {
		final String result;

		switch (file.getType()) {
		case INDEXED:
			result = null;
			break;
		case RELATIVE:
			result = null;
			break;
		case SEQUENTIAL:
		default:
			result = sequentialFileAccessService.read((CobolSequentialFile) file, addresses, criterion, programUnit);
			break;
		}

		return result;
	}

	@Override
	public void rewrite(final String record, final CobolFile file, final FileControlEntry fileControlEntry) {
		switch (file.getType()) {
		case INDEXED:
			break;
		case RELATIVE:
			break;
		case SEQUENTIAL:
		default:
			sequentialFileAccessService.rewrite(record, (CobolSequentialFile) file);
			break;
		}
	}

	@Override
	public void sort(final CobolFile file, final List<CobolAddress> addresses, final List<SortCriterion> criteria,
			final FileControlEntry fileControlEntry, final ProgramUnit programUnit) {
		switch (file.getType()) {
		case INDEXED:
			break;
		case RELATIVE:
			break;
		case SEQUENTIAL:
		default:
			sequentialFileAccessService.sort((CobolSequentialFile) file, addresses, criteria, programUnit);
			break;
		}
	}

	@Override
	public void start(final CobolFile file, final FileControlEntry fileControlEntry) {
		switch (file.getType()) {
		case INDEXED:
			break;
		case RELATIVE:
			break;
		case SEQUENTIAL:
		default:
			sequentialFileAccessService.start((CobolSequentialFile) file);
			break;
		}
	}

	@Override
	public void write(final String record, final CobolFile file, final FileControlEntry fileControlEntry) {
		switch (file.getType()) {
		case INDEXED:
			break;
		case RELATIVE:
			break;
		case SEQUENTIAL:
		default:
			sequentialFileAccessService.write(record, (CobolSequentialFile) file);
			break;
		}
	}
}
