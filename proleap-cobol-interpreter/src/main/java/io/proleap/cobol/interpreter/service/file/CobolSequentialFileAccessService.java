package io.proleap.cobol.interpreter.service.file;

import java.util.List;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.file.CobolSequentialFile;
import io.proleap.cobol.interpreter.service.file.domain.KeyCriterion;
import io.proleap.cobol.interpreter.service.file.domain.SortCriterion;

public interface CobolSequentialFileAccessService {

	void close(CobolSequentialFile file);

	void copy(CobolSequentialFile from, CobolSequentialFile to);

	void delete(CobolSequentialFile file, List<CobolAddress> addresses, KeyCriterion keyCriterion,
			ProgramUnit programUnit);

	void open(CobolSequentialFile file);

	String read(CobolSequentialFile file, List<CobolAddress> addresses, KeyCriterion keyCriterion,
			ProgramUnit programUnit);

	void rewrite(String record, CobolSequentialFile file);

	void sort(CobolSequentialFile file, List<CobolAddress> addresses, List<SortCriterion> sortCriteria,
			ProgramUnit programUnit);

	void start(CobolSequentialFile file);

	void write(String record, CobolSequentialFile file);
}
