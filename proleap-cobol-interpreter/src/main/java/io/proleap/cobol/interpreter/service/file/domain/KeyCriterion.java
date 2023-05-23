package io.proleap.cobol.interpreter.service.file.domain;

import java.util.ArrayList;
import java.util.List;

import io.proleap.cobol.commons.value.domain.CobolValue;

public class KeyCriterion {

	public CobolValue keyValue;

	public List<Integer> positions = new ArrayList<>();
}
