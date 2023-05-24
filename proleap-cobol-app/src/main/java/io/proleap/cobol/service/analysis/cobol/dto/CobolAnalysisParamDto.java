package io.proleap.cobol.service.analysis.cobol.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CobolAnalysisParamDto {

	@JsonProperty("code")
	public String code;

	@JsonProperty("format")
	public String format;
}
