package io.proleap.cobol.analysis.issues.dto;

public enum SeverityEnum {
	BLOCKER(4), CRITICAL(3), INFO(0), MAJOR(2), MINOR(1);

	final int severity;

	private SeverityEnum(final int severity) {
		this.severity = severity;
	}
}
