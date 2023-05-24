package io.proleap.cobol.service.mail.impl;

import java.util.Properties;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.MimeMessage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.proleap.cobol.service.config.SmtpConfig;
import io.proleap.cobol.service.mail.MailService;

@Singleton
public class MailServiceImpl implements MailService {

	private final static Logger LOG = LoggerFactory.getLogger(MailServiceImpl.class);

	@Inject
	private SmtpConfig smtpConfig;

	@Override
	public MimeMessage createMimeMessage() {
		final Session session = createSession();
		return new MimeMessage(session);
	}

	private Session createSession() {
		final Properties props = new Properties();
		props.put("mail.transport.protocol", "smtp");
		props.put("mail.smtp.auth", "true");
		props.put("mail.smtp.starttls.enable", "true");
		props.put("mail.smtp.allow8bitmime", "true");
		props.put("mail.smtps.allow8bitmime", "true");

		return Session.getDefaultInstance(props);
	}

	protected Transport createTransport() throws MessagingException {
		final Session session = createSession();
		return session.getTransport();
	}

	protected boolean isValidSmtpConfig() {
		return smtpConfig.getSmtpHost() != null && !smtpConfig.getSmtpHost().isEmpty()
				&& smtpConfig.getSmtpUsername() != null && !smtpConfig.getSmtpUsername().isEmpty()
				&& smtpConfig.getSmtpPassword() != null && !smtpConfig.getSmtpPassword().isEmpty();
	}

	@Override
	public boolean sendMimeMessage(final MimeMessage mimeMessage) {
		try {
			if (!isValidSmtpConfig()) {
				// sic! no logging of email address due to data protection laws
				LOG.warn("Cannot not send mail due to insufficient SMTP config\nSubject: {}\n\n{}",
						mimeMessage.getSubject(), mimeMessage.getContent());
				return false;
			} else {
				LOG.info("Sending mime message");

				// saveChanges is mandatory, otherwise the mail cannot be sent
				mimeMessage.saveChanges();

				final Transport transport = createTransport();
				transport.connect(smtpConfig.getSmtpHost(), smtpConfig.getSmtpPort(), smtpConfig.getSmtpUsername(),
						smtpConfig.getSmtpPassword());
				transport.sendMessage(mimeMessage, mimeMessage.getAllRecipients());
				transport.close();

				return true;
			}
		} catch (final Exception e) {
			LOG.warn(e.getMessage(), e);
			return false;
		}
	}
}
