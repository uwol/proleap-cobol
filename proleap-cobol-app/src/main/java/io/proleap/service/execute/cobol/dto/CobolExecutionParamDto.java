package io.proleap.service.execute.cobol.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CobolExecutionParamDto {

	@JsonProperty("code")
	public String code;

	@JsonProperty("format")
	public String format;
}
