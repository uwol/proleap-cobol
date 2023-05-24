package io.proleap.cobol.servlets.web;

import static io.proleap.cobol.context.ApplicationContextUtils.getBean;

import java.io.IOException;
import java.net.HttpURLConnection;

import javax.mail.internet.MimeMessage;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.proleap.cobol.service.config.SmtpConfig;
import io.proleap.cobol.service.mail.MailService;
import io.proleap.cobol.servlets.AbstractServlet;
import io.proleap.cobol.servlets.web.dto.WebMessageDto;
import io.proleap.cobol.util.MailUtils;

@WebServlet(WebMailServlet.PATTERN)
public class WebMailServlet extends AbstractServlet {

	private final static Logger LOG = LoggerFactory.getLogger(WebMailServlet.class);

	public static final String PATTERN = "/web/mail";

	private static final long serialVersionUID = 1L;

	@Override
	protected void doPost(final HttpServletRequest req, final HttpServletResponse resp)
			throws ServletException, IOException {
		enableCors(req, resp);
		disableCache(resp);
		setContentTypeApplicationJson(resp);

		boolean success = false;

		try {
			final WebMessageDto dto = readJson(req, WebMessageDto.class);
			final MimeMessage mimeMessage = getBean(MailService.class).createMimeMessage();

			final String from = getBean(SmtpConfig.class).getSmtpFrom();
			final String fromPersonal = getBean(SmtpConfig.class).getSmtpFromPersonal();
			final String subject = String.format("Email via website from %s<%s> with phone %s", dto.name, dto.email,
					dto.phone);
			final String text = dto.message;
			final String to = getBean(SmtpConfig.class).getSmtpToContact();

			MailUtils.setFrom(from, fromPersonal, mimeMessage);
			MailUtils.setSubject(subject, mimeMessage);
			MailUtils.setTextPlain(text, mimeMessage);
			MailUtils.setTo(to, mimeMessage);

			success = getBean(MailService.class).sendMimeMessage(mimeMessage);
		} catch (final Exception e) {
			LOG.warn(e.getMessage());
		}

		if (success) {
		} else {
			resp.setStatus(HttpURLConnection.HTTP_INTERNAL_ERROR);
			writeBody("false", resp);
		}
	}
}
