package io.proleap.service.execute.cobol.dto;

import java.util.ArrayList;
import java.util.List;

public class CobolExecutionResultDto {

	public List<String> output = new ArrayList<>();

	public List<ExecutionTupleDto> values = new ArrayList<>();
}
