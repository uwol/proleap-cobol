package io.proleap.cobol.transform.java.identifier.method.impl;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.transform.java.identifier.JavaIdentifierService;
import io.proleap.cobol.transform.java.identifier.method.JavaMethodIdentifierService;

@Singleton
public class JavaMethodIdentifierServiceImpl implements JavaMethodIdentifierService {

	@Inject
	private JavaIdentifierService javaIdentifierService;

	@Override
	public String mapToIdentifier(final Paragraph paragraph) {
		return javaIdentifierService.mapToIdentifier(paragraph.getName());
	}

	@Override
	public String mapToIdentifier(final Section section) {
		return javaIdentifierService.mapToIdentifier(section.getName());
	}
}
