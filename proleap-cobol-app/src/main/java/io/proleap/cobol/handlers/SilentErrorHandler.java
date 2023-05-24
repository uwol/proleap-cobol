package io.proleap.cobol.handlers;

import java.io.IOException;
import java.io.Writer;

import javax.servlet.http.HttpServletRequest;

import org.eclipse.jetty.server.handler.ErrorHandler;

public class SilentErrorHandler extends ErrorHandler {

	@Override
	protected void writeErrorPage(final HttpServletRequest request, final Writer writer, final int code,
			final String message, final boolean showStacks) throws IOException {
	}
}
