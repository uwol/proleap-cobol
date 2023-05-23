package io.proleap.cobol.interpreter.service.file;

import java.util.List;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.service.file.domain.KeyCriterion;
import io.proleap.cobol.interpreter.service.file.domain.SortCriterion;

public interface CobolFileAccessService {

	CobolFile assureFile(FileControlEntry fileControlEntry, CobolState state);

	void close(CobolFile file, FileControlEntry fileControlEntry);

	void copy(CobolFile file1, CobolFile file2, FileControlEntry fileControlEntry);

	void delete(CobolFile file, List<CobolAddress> addresses, KeyCriterion criterion, FileControlEntry fileControlEntry,
			ProgramUnit programUnit);

	void open(CobolFile file, FileControlEntry fileControlEntry);

	String read(CobolFile file, List<CobolAddress> addresses, KeyCriterion criterion, FileControlEntry fileControlEntry,
			ProgramUnit programUnit);

	void rewrite(String record, CobolFile file, FileControlEntry fileControlEntry);

	void sort(CobolFile file, List<CobolAddress> addresses, List<SortCriterion> criteria,
			FileControlEntry fileControlEntry, ProgramUnit programUnit);

	void start(CobolFile file, FileControlEntry fileControlEntry);

	void write(String record, CobolFile file, FileControlEntry fileControlEntry);
}
