package io.proleap.service.mail;

import javax.mail.internet.MimeMessage;

public interface MailService {

	MimeMessage createMimeMessage();

	boolean sendMimeMessage(MimeMessage mimeMessage);
}
