package io.proleap.cobol.log;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;

public class ProLeapThreadLocalLogAppender extends AppenderBase<ILoggingEvent> {

	public static final ThreadLocal<StringBuffer> LOG_THREAD_LOCAL = new ThreadLocal<StringBuffer>();

	private static final String NEWLINE = "\n";

	protected static void assureStringBuffer() {
		if (LOG_THREAD_LOCAL.get() == null) {
			LOG_THREAD_LOCAL.set(new StringBuffer());
		}
	}

	public static String getLog() {
		assureStringBuffer();

		final StringBuffer sb = LOG_THREAD_LOCAL.get();
		return sb.toString();
	}

	public static void reset() {
		LOG_THREAD_LOCAL.set(null);
	}

	@Override
	public void append(final ILoggingEvent logEvent) {
		assureStringBuffer();

		final String formattedMessage = logEvent.getFormattedMessage();
		LOG_THREAD_LOCAL.get().append(formattedMessage + NEWLINE);
	}
}
