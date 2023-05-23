package io.proleap.util;

import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MailUtils {

	private static final String CONTENT_TYPE_CHARSET_SUFFIX = ";charset=";

	private static final String CONTENT_TYPE_HTML = "text/html";

	private final static Logger LOG = LoggerFactory.getLogger(MailUtils.class);

	public static void setBcc(final String bcc, final MimeMessage mimeMessage) {
		if (bcc != null && !bcc.isEmpty()) {
			try {
				mimeMessage.addRecipient(Message.RecipientType.BCC, new InternetAddress(bcc));
			} catch (final MessagingException e) {
				LOG.warn(e.getMessage());
			}
		}
	}

	public static void setCc(final String cc, final MimeMessage mimeMessage) {
		if (cc != null && !cc.isEmpty()) {
			try {
				mimeMessage.addRecipient(Message.RecipientType.CC, new InternetAddress(cc));
			} catch (final MessagingException e) {
				LOG.warn(e.getMessage());
			}
		}
	}

	public static void setFrom(final String from, final MimeMessage mimeMessage) {
		if (from != null && !from.isEmpty()) {
			try {
				mimeMessage.setFrom(new InternetAddress(from));
			} catch (final MessagingException e) {
				LOG.warn(e.getMessage());
			}
		}
	}

	public static void setFrom(final String from, final String fromPersonal, final MimeMessage mimeMessage) {
		try {
			if (from != null && !from.isEmpty() && fromPersonal != null && !fromPersonal.isEmpty()) {
				mimeMessage.setFrom(new InternetAddress(from, fromPersonal, StandardCharsets.UTF_8.toString()));
			} else if (from != null && !from.isEmpty()) {
				mimeMessage.setFrom(new InternetAddress(from));
			}
		} catch (final MessagingException e) {
			LOG.warn(e.getMessage());
		} catch (final UnsupportedEncodingException e) {
			LOG.warn(e.getMessage());
		}
	}

	public static void setReplyTo(final String replyTo, final MimeMessage mimeMessage) {
		if (replyTo != null && !replyTo.isEmpty()) {
			try {
				final InternetAddress address = new InternetAddress(replyTo);
				mimeMessage.setReplyTo(new InternetAddress[] { address });
			} catch (final MessagingException e) {
				LOG.warn(e.getMessage());
			}
		}
	}

	public static void setSubject(final String subject, final MimeMessage mimeMessage) {
		if (subject != null && !subject.isEmpty()) {
			try {
				mimeMessage.setSubject(subject, StandardCharsets.UTF_8.toString());
			} catch (final MessagingException e) {
				LOG.warn(e.getMessage());
			}
		}
	}

	public static void setTextHtml(final String text, final MimeMessage mimeMessage) {
		if (text != null && !text.isEmpty()) {
			try {
				mimeMessage.setContent(text,
						CONTENT_TYPE_HTML + CONTENT_TYPE_CHARSET_SUFFIX + StandardCharsets.UTF_8.toString());
			} catch (final MessagingException e) {
				LOG.warn(e.getMessage());
			}
		}
	}

	public static void setTextPlain(final String text, final MimeMessage mimeMessage) {
		if (text != null && !text.isEmpty()) {
			try {
				mimeMessage.setText(text, StandardCharsets.UTF_8.toString());
			} catch (final MessagingException e) {
				LOG.warn(e.getMessage());
			}
		}
	}

	public static void setTo(final String to, final MimeMessage mimeMessage) {
		if (to != null && !to.isEmpty()) {
			try {
				mimeMessage.setRecipient(Message.RecipientType.TO, new InternetAddress(to));
			} catch (final MessagingException e) {
				LOG.warn(e.getMessage());
			}
		}
	}
}
