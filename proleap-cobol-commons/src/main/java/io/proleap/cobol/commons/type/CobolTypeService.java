package io.proleap.cobol.commons.type;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.PictureClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.ValueClause;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;

public interface CobolTypeService {

	CobolTypeEnum getType(Call call);

	CobolTypeEnum getType(DataDescriptionEntry dataDescriptionEntry);

	CobolTypeEnum getType(PictureClause pictureClause);

	CobolTypeEnum getType(ValueClause valueClause);

	CobolTypeEnum getType(ValueStmt valueStmt);
}
