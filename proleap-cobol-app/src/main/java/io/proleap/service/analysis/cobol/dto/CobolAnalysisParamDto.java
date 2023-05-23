package io.proleap.service.analysis.cobol.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CobolAnalysisParamDto {

	@JsonProperty("code")
	public String code;

	@JsonProperty("format")
	public String format;
}
