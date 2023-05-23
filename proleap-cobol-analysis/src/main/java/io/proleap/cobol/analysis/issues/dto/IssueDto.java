package io.proleap.cobol.analysis.issues.dto;

public class IssueDto implements Comparable<IssueDto> {

	public final String context;

	public final String description;

	public final String href;

	public final int severity;

	public IssueDto(final String context, final String description, final SeverityEnum severity, final String href) {
		this.context = context;
		this.description = description;
		this.severity = severity.severity;
		this.href = href;
	}

	@Override
	public int compareTo(final IssueDto o) {
		return o.severity - severity;
	}
}
