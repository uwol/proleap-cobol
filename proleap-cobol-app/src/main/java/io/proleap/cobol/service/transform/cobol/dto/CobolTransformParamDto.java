package io.proleap.cobol.service.transform.cobol.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CobolTransformParamDto {

	@JsonProperty("code")
	public String code;

	@JsonProperty("format")
	public String format;
}
