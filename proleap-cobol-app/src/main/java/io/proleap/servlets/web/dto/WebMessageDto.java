package io.proleap.servlets.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class WebMessageDto {

	@JsonProperty("email")
	public String email;

	@JsonProperty("message")
	public String message;

	@JsonProperty("name")
	public String name;

	@JsonProperty("phone")
	public String phone;
}
