package io.proleap.cobol.service.mail.impl;

import java.io.PrintWriter;
import java.io.StringWriter;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.mail.internet.MimeMessage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.proleap.cobol.service.config.SmtpConfig;
import io.proleap.cobol.service.mail.DevOpsMailService;
import io.proleap.cobol.service.mail.MailService;
import io.proleap.cobol.util.MailUtils;

@Singleton
public class DevOpsMailServiceImpl implements DevOpsMailService {

	private final static Logger LOG = LoggerFactory.getLogger(DevOpsMailServiceImpl.class);

	@Inject
	private MailService mailService;

	@Inject
	private SmtpConfig smtpConfig;

	@Override
	public boolean sendDevOpsMessage(final String subject, final String text) {
		try {
			final MimeMessage mimeMessage = mailService.createMimeMessage();

			final String from = smtpConfig.getSmtpFrom();
			final String fromPersonal = smtpConfig.getSmtpFromPersonal();
			final String to = smtpConfig.getSmtpToDevOps();

			MailUtils.setFrom(from, fromPersonal, mimeMessage);
			MailUtils.setSubject(subject, mimeMessage);
			MailUtils.setTextPlain(text, mimeMessage);
			MailUtils.setTo(to, mimeMessage);

			return mailService.sendMimeMessage(mimeMessage);
		} catch (final Exception e) {
			LOG.warn(e.getMessage());
		}

		return false;
	}

	@Override
	public boolean sendExceptionMessage(final Exception e) {
		final StringWriter errors = new StringWriter();
		e.printStackTrace(new PrintWriter(errors));

		return sendDevOpsMessage("[ProLeap] Exception", String.valueOf(errors));
	}
}
