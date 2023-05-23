package io.proleap.cobol.commons.datadescription;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;

public interface CobolPictureLengthService {

	Integer getFractionalPartLength(String pictureString);

	Integer getIntegerPartLength(String pictureString);

	Integer getLength(DataDescriptionEntry dataDescriptionEntry);

	Integer getLength(String pictureString);

	Integer getStringLength(String pictureString);
}
