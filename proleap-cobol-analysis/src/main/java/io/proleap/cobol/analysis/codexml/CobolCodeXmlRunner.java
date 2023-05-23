package io.proleap.cobol.analysis.codexml;

import java.io.IOException;
import java.util.List;

import org.dom4j.Document;

import io.proleap.cobol.analysis.registry.CobolIdRegistry;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;

public interface CobolCodeXmlRunner {

	Document analyzeCompilationUnit(CompilationUnit compilationUnit, CobolIdRegistry idRegistry) throws IOException;

	List<Document> analyzeProgram(Program program, CobolIdRegistry idRegistry) throws IOException;
}
