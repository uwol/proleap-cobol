package io.proleap.cobol.interpreter.service.file.criteria;

import java.util.List;

import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.AlternateRecordKeyClause;
import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.RecordKeyClause;
import io.proleap.cobol.asg.metamodel.procedure.read.Key;
import io.proleap.cobol.asg.metamodel.procedure.sort.OnKey;
import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;
import io.proleap.cobol.interpreter.service.file.domain.KeyCriterion;
import io.proleap.cobol.interpreter.service.file.domain.SortCriterion;

public interface CobolFileCriteriaService {

	KeyCriterion createKeyCriterion(AlternateRecordKeyClause alternateRecordKeyClause, List<CobolAddress> addresses,
			CobolInterpreterParams params);

	KeyCriterion createKeyCriterion(Key key, List<CobolAddress> addresses, CobolInterpreterParams params);

	KeyCriterion createKeyCriterion(RecordKeyClause recordKeyClause, List<CobolAddress> addresses,
			CobolInterpreterParams params);

	List<SortCriterion> createSortCriteria(List<CobolAddress> addresses, List<OnKey> onKeys,
			CobolInterpreterParams params);
}
