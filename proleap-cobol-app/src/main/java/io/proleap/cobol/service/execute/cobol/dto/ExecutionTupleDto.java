package io.proleap.cobol.service.execute.cobol.dto;

public class ExecutionTupleDto implements Comparable<ExecutionTupleDto> {

	public String key;

	public String value;

	@Override
	public int compareTo(final ExecutionTupleDto o) {
		return key.compareTo(o.key);
	}
}
