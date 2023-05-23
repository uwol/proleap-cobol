package io.proleap.cobol.commons.datadescription;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.commons.value.CobolValueProvider;
import io.proleap.cobol.commons.value.domain.CobolValue;

public interface CobolValueProviderService {

	CobolValue getValue(DataDescriptionEntry dataDescriptionEntry, CobolValueProvider valueProvider);
}
