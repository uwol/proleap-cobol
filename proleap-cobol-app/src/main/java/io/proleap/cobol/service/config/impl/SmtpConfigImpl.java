package io.proleap.cobol.service.config.impl;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.env.EnvService;
import io.proleap.cobol.service.config.SmtpConfig;

@Singleton
public class SmtpConfigImpl implements SmtpConfig {

	@Inject
	private EnvService envService;

	@Override
	public String getSmtpFrom() {
		return envService.getEnv("SMTP_FROM", "");
	}

	@Override
	public String getSmtpFromPersonal() {
		return envService.getEnv("SMTP_FROM_NAME", "");
	}

	@Override
	public String getSmtpHost() {
		return envService.getEnv("SMTP_HOST", "");
	}

	@Override
	public String getSmtpPassword() {
		return envService.getEnv("SMTP_PASSWORD", "");
	}

	@Override
	public Integer getSmtpPort() {
		return Integer.valueOf(envService.getEnv("SMTP_PORT", "587"));
	}

	@Override
	public String getSmtpToContact() {
		return envService.getEnv("SMTP_TO_CONTACT", "");
	}

	@Override
	public String getSmtpToDevOps() {
		return envService.getEnv("SMTP_TO_DEVOPS", "");
	}

	@Override
	public String getSmtpUsername() {
		return envService.getEnv("SMTP_USERNAME", "");
	}
}
