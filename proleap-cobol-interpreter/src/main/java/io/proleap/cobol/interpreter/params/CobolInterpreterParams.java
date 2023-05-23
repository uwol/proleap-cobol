package io.proleap.cobol.interpreter.params;

import java.io.File;
import java.util.List;
import java.util.Queue;

import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;
import io.proleap.cobol.interpreter.domain.state.CobolState;

public interface CobolInterpreterParams {

	Queue<String> getAcceptParams();

	List<String> getCobolFileExtensions();

	List<File> getCobolFiles();

	CobolAddressGroup getGivingParam();

	CobolParserParams getParserParams();

	List<CobolAddressGroup> getReferenceParams();

	CobolState getState();

	List<CobolAddressGroup> getValueParams();

	void setAcceptParams(List<String> acceptParams);

	void setCobolFileExtensions(List<String> cobolFileExtensions);

	void setCobolFiles(List<File> cobolFiles);

	void setGivingParam(CobolAddressGroup givingParam);

	void setParserParams(CobolParserParams parserParams);

	void setReferenceParams(List<CobolAddressGroup> referenceParams);

	void setState(CobolState state);

	void setValueParams(List<CobolAddressGroup> valueParams);
}
