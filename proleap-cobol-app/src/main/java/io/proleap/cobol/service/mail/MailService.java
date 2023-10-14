package io.proleap.cobol.service.mail;

import jakarta.mail.internet.MimeMessage;

public interface MailService {

	MimeMessage createMimeMessage();

	boolean sendMimeMessage(MimeMessage mimeMessage);
}
