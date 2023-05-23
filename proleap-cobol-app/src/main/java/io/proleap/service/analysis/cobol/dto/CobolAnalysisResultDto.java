package io.proleap.service.analysis.cobol.dto;

import java.util.ArrayList;
import java.util.List;

import io.proleap.cobol.analysis.issues.dto.IssueDto;

public class CobolAnalysisResultDto {

	public String codeView;

	public final List<IssueDto> issues = new ArrayList<>();
}
