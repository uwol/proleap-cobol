package io.proleap.cobol.interpreter.params.impl;

import java.io.File;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.params.CobolInterpreterParams;

public class CobolInterpreterParamsImpl implements CobolInterpreterParams {

	protected Queue<String> acceptParams;

	protected List<String> cobolFileExtensions;

	protected List<File> cobolFiles;

	protected CobolAddressGroup givingParam;

	protected CobolParserParams parserParams;

	protected List<CobolAddressGroup> referenceParams;

	protected CobolState state;

	protected List<CobolAddressGroup> valueParams;

	@Override
	public Queue<String> getAcceptParams() {
		return acceptParams;
	}

	@Override
	public List<String> getCobolFileExtensions() {
		return cobolFileExtensions;
	}

	@Override
	public List<File> getCobolFiles() {
		return cobolFiles;
	}

	@Override
	public CobolAddressGroup getGivingParam() {
		return givingParam;
	}

	@Override
	public CobolParserParams getParserParams() {
		return parserParams;
	}

	@Override
	public List<CobolAddressGroup> getReferenceParams() {
		return referenceParams;
	}

	@Override
	public CobolState getState() {
		return state;
	}

	@Override
	public List<CobolAddressGroup> getValueParams() {
		return valueParams;
	}

	@Override
	public void setAcceptParams(final List<String> acceptParams) {
		this.acceptParams = new LinkedList<>(acceptParams);
	}

	@Override
	public void setCobolFileExtensions(final List<String> cobolFileExtensions) {
		this.cobolFileExtensions = cobolFileExtensions;
	}

	@Override
	public void setCobolFiles(final List<File> cobolFiles) {
		this.cobolFiles = cobolFiles;
	}

	@Override
	public void setGivingParam(final CobolAddressGroup givingParam) {
		this.givingParam = givingParam;
	}

	@Override
	public void setParserParams(final CobolParserParams parserParams) {
		this.parserParams = parserParams;
	}

	@Override
	public void setReferenceParams(final List<CobolAddressGroup> referenceParams) {
		this.referenceParams = referenceParams;
	}

	@Override
	public void setState(final CobolState state) {
		this.state = state;
	}

	@Override
	public void setValueParams(final List<CobolAddressGroup> valueParams) {
		this.valueParams = valueParams;
	}
}
