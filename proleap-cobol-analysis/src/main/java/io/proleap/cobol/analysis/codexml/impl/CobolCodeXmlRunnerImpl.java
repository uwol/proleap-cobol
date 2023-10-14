package io.proleap.cobol.analysis.codexml.impl;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import jakarta.inject.Singleton;

import org.dom4j.Document;
import org.dom4j.DocumentHelper;

import io.proleap.cobol.analysis.codexml.CobolCodeXmlRunner;
import io.proleap.cobol.analysis.codexml.CobolCodeXmlVisitor;
import io.proleap.cobol.analysis.registry.CobolIdRegistry;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;

@Singleton
public class CobolCodeXmlRunnerImpl implements CobolCodeXmlRunner {

	@Override
	public Document analyzeCompilationUnit(final CompilationUnit compilationUnit, final CobolIdRegistry idRegistry)
			throws IOException {
		final Document document = DocumentHelper.createDocument();
		document.setXMLEncoding(StandardCharsets.UTF_8.toString());
		document.setName(compilationUnit.getName());

		final CobolCodeXmlVisitor visitor = new CobolCodeXmlVisitor(compilationUnit, document, idRegistry);
		visitor.visit(compilationUnit.getCtx());

		return document;
	}

	@Override
	public List<Document> analyzeProgram(final Program program, final CobolIdRegistry idRegistry) throws IOException {
		final List<Document> result = new ArrayList<Document>();

		for (final CompilationUnit compilationUnit : program.getCompilationUnits()) {
			final Document document = analyzeCompilationUnit(compilationUnit, idRegistry);
			result.add(document);
		}

		return result;
	}
}
