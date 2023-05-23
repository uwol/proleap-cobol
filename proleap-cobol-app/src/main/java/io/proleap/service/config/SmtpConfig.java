package io.proleap.service.config;

public interface SmtpConfig {

	String getSmtpFrom();

	String getSmtpFromPersonal();

	String getSmtpHost();

	String getSmtpPassword();

	Integer getSmtpPort();

	String getSmtpToContact();

	String getSmtpToDevOps();

	String getSmtpUsername();
}
