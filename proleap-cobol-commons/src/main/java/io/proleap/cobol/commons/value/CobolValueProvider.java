package io.proleap.cobol.commons.value;

import java.util.List;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.commons.value.domain.CobolValue;

public interface CobolValueProvider {

	List<CobolValue> getValues(DataDescriptionEntry key);
}
