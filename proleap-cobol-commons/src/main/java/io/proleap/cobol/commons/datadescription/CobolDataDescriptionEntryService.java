package io.proleap.cobol.commons.datadescription;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;

/**
 * Service for resolving and analyzing data description entries.
 */
public interface CobolDataDescriptionEntryService {

	DataDescriptionEntry findEffectiveDataDescriptionEntry(DataDescriptionEntry dataDescriptionEntry);

	DataDescriptionEntry getDataDescriptionEntry(Call call);

	DataDescriptionEntry getDataDescriptionEntry(ValueStmt valueStmt);

	boolean hasChildren(DataDescriptionEntry dataDescriptionEntry);
}
