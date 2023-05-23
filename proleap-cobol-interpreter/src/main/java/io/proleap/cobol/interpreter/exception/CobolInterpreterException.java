package io.proleap.cobol.interpreter.exception;

public class CobolInterpreterException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public CobolInterpreterException(final Exception e) {
		super(e);
	}

	public CobolInterpreterException(final String message) {
		super(message);
	}
}
