package io.proleap.cobol.transform.java.runner.impl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import org.antlr.v4.runtime.ParserRuleContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.transform.java.printer.TypedPrinter;
import io.proleap.cobol.transform.java.printer.impl.TypedPrinterImpl;
import io.proleap.cobol.transform.java.runner.CobolTransformationRunner;
import io.proleap.cobol.transform.java.type.JavaTypeService;
import io.proleap.cobol.transform.printer.Printer;
import io.proleap.cobol.transform.printer.impl.PrinterImpl;
import io.proleap.cobol.transform.rule.CobolTransformRuleMatcher;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class CobolTransformationRunnerImpl implements CobolTransformationRunner {

	private final static Logger LOG = LoggerFactory.getLogger(CobolTransformationRunnerImpl.class);

	@Inject
	protected JavaTypeService javaTypeService;

	@Inject
	protected CobolTransformRuleMatcher ruleMatcher;

	protected List<File> transform(final Program program, final String packageName) throws IOException {
		final List<File> result = new ArrayList<>();

		for (final CompilationUnit compilationUnit : program.getCompilationUnits()) {
			final String compilationUnitName = compilationUnit.getName();

			LOG.info("Transforming compilation unit {}.", compilationUnitName);

			// open output file
			final File outputFile = Files.createTempFile(javaTypeService.mapToType(compilationUnitName), "java")
					.toFile();

			// rule context
			final RuleContext ruleContext = new RuleContext();

			// set output file on printer
			final Printer printer = new PrinterImpl(outputFile);

			// typed printer
			final TypedPrinter typedPrinter = new TypedPrinterImpl(ruleContext);

			// init rule context
			ruleContext.setProgram(program);
			ruleContext.setPackageName(packageName);
			ruleContext.setRuleMatcher(ruleMatcher);
			ruleContext.setPrinter(printer);
			ruleContext.setTypedPrinter(typedPrinter);

			// transform
			final ParserRuleContext ctx = compilationUnit.getCtx();
			final Object semanticGraphElement = program.getASGElementRegistry().getASGElement(ctx);
			ruleMatcher.apply(ctx, semanticGraphElement, ruleContext);

			// close printer
			printer.flush();
			printer.close();

			result.add(outputFile);
		}

		return result;
	}

	@Override
	public List<File> transformCode(final String cobolCode, final String compilationUnitName, final String packageName,
			final CobolParserParams params) throws IOException {
		final Program program = new CobolParserRunnerImpl().analyzeCode(cobolCode, compilationUnitName, params);
		return transform(program, packageName);
	}

	@Override
	public List<File> transformFile(final File cobolFile, final String packageName, final CobolParserParams params)
			throws IOException {
		final Program program = new CobolParserRunnerImpl().analyzeFile(cobolFile, params);
		return transform(program, packageName);
	}
}
