package io.proleap.cobol;

import java.util.EnumSet;

import jakarta.servlet.DispatcherType;

import org.eclipse.jetty.server.Handler;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.ErrorHandler;
import org.eclipse.jetty.server.handler.HandlerList;
import org.eclipse.jetty.webapp.Configuration;
import org.eclipse.jetty.webapp.FragmentConfiguration;
import org.eclipse.jetty.webapp.WebAppContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.Level;
import io.proleap.cobol.filters.ErrorHandlerFilter;
import io.proleap.cobol.handlers.SilentErrorHandler;
import io.proleap.cobol.log.ProLeapThreadLocalLogAppender;
import io.proleap.cobol.servlets.ServiceStatusServlet;
import io.proleap.cobol.servlets.cobol.analyze.CobolTextAnalyzeServlet;
import io.proleap.cobol.servlets.cobol.execute.CobolTextExecuteServlet;
import io.proleap.cobol.servlets.cobol.transform.CobolTextTransformServlet;
import io.proleap.cobol.servlets.cron.WarmupCronServlet;
import io.proleap.cobol.servlets.web.WebMailServlet;

public class ProLeapCobolEmbeddedJetty {

	private static final int DEFAULT_PORT = 4567;

	private final static Logger LOG = LoggerFactory.getLogger(ProLeapCobolEmbeddedJetty.class);

	private static final String PORT = "PORT";

	private static void configureLogAppenders() {
		LOG.info("Configuring ProLeap API log appender.");

		final ch.qos.logback.classic.Logger logbackLogger = (ch.qos.logback.classic.Logger) LoggerFactory
				.getLogger(Logger.ROOT_LOGGER_NAME);
		logbackLogger.setLevel(Level.INFO);

		final ProLeapThreadLocalLogAppender proLeapThreadLocalLogAppender = new ProLeapThreadLocalLogAppender();
		proLeapThreadLocalLogAppender.start();
		logbackLogger.addAppender(proLeapThreadLocalLogAppender);
	}

	public static void main(final String[] args) throws Exception {
		configureLogAppenders();

		final int port = System.getenv(PORT) != null ? Integer.valueOf(System.getenv(PORT)) : DEFAULT_PORT;
		final Server server = new Server(port);

		final ErrorHandler errorHandler = new SilentErrorHandler();
		errorHandler.setShowStacks(false);

		final WebAppContext context = new WebAppContext();
		context.setContextPath("/");
		context.setErrorHandler(errorHandler);

		context.addFilter(ErrorHandlerFilter.class, ErrorHandlerFilter.PATTERN, EnumSet.of(DispatcherType.REQUEST));

		context.addServlet(CobolTextAnalyzeServlet.class, CobolTextAnalyzeServlet.PATTERN);
		context.addServlet(CobolTextExecuteServlet.class, CobolTextExecuteServlet.PATTERN);
		context.addServlet(CobolTextTransformServlet.class, CobolTextTransformServlet.PATTERN);

		// context.addServlet(CobolZipAnalyzeServlet.class,
		// CobolZipAnalyzeServlet.PATTERN).getRegistration()
		// .setMultipartConfig(new MultipartConfigElement(""));

		// context.addServlet(CobolZipTransformServlet.class,
		// CobolZipTransformServlet.PATTERN).getRegistration()
		// .setMultipartConfig(new MultipartConfigElement(""));

		context.addServlet(ServiceStatusServlet.class, ServiceStatusServlet.PATTERN);
		context.addServlet(WarmupCronServlet.class, WarmupCronServlet.PATTERN);
		context.addServlet(WebMailServlet.class, WebMailServlet.PATTERN);

		final HandlerList handlers = new HandlerList();
		handlers.setHandlers(new Handler[] { context });

		context.setConfigurations(new Configuration[] { new FragmentConfiguration() });

		server.addBean(errorHandler);

		server.setHandler(handlers);
		server.start();
		server.join();
	}
}
