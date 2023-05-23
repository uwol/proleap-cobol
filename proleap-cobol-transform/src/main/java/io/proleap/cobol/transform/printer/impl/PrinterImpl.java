package io.proleap.cobol.transform.printer.impl;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.antlr.v4.runtime.ParserRuleContext;

import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.ProgramUnitElement;
import io.proleap.cobol.asg.util.StringUtils;
import io.proleap.cobol.transform.printer.LinesHolder;
import io.proleap.cobol.transform.printer.Printer;

public class PrinterImpl implements Printer {

	private static final String COMMENT = "//";

	private static final int LINE_LENGTH = 100;

	private static final boolean PRINT_EMPTY_COMMENTS = false;

	private static final String WS = " ";

	private static final int WS_PER_INDENT = 4;

	protected int indentation = 0;

	protected Integer lastSourceLineNumber = null;

	protected StringBuffer lineBuffer = new StringBuffer();

	protected Map<CompilationUnit, LinesHolder> linesHolders = new HashMap<CompilationUnit, LinesHolder>();

	protected PrintWriter printWriter;

	public PrinterImpl(final File outputFile) throws IOException {
		printWriter = new PrintWriter(new FileWriter(outputFile));
	}

	protected void assureLinesHolder(final CompilationUnit compilationUnit) {
		if (linesHolders.get(compilationUnit) == null) {
			final List<String> lines = compilationUnit.getLines();
			linesHolders.put(compilationUnit, new LinesHolderImpl(lines));
		}
	}

	@Override
	public void close() {
		printWriter.close();
	}

	protected String createComment(final int lineNumber, final String sourceLine) {
		final String result;

		if (PRINT_EMPTY_COMMENTS || !(sourceLine == null || sourceLine.isEmpty())) {
			final String lineNumberString = StringUtils.leftPad("(" + (lineNumber + 1) + ")", 2 + 3);
			return WS + COMMENT + WS + lineNumberString + WS + sourceLine;
		} else {
			result = "";
		}

		return result;
	}

	protected String createIndentation() {
		return WS.repeat(indentation * WS_PER_INDENT);
	}

	@Override
	public void flush() {
		printWriter.flush();
	}

	protected Integer getSourceLineNumber(final ProgramUnitElement element) {
		final ParserRuleContext ctx = element.getCtx();
		final int result = ctx.start.getLine() - 1;
		return result;
	}

	@Override
	public void indent() {
		indentation++;
	}

	@Override
	public void print(final String str) {
		lineBuffer.append(str);
	}

	@Override
	public void print(final String format, final Object... args) {
		final String str = String.format(format, args);
		print(str);
	}

	protected void printFillerLinesBefore(final Integer endSourceLineNumber, final CompilationUnit compilationUnit) {
		for (int sourceLineNumber = lastSourceLineNumber
				+ 1; sourceLineNumber < endSourceLineNumber; sourceLineNumber++) {
			final String line = linesHolders.get(compilationUnit).getLine(sourceLineNumber);

			if (PRINT_EMPTY_COMMENTS || !(line == null || line.isEmpty())) {
				final String comment = createComment(sourceLineNumber, line);
				printWriter.println(WS.repeat(LINE_LENGTH) + comment);
			}

			lastSourceLineNumber = sourceLineNumber;
		}
	}

	@Override
	public void printNewline() {
		printNewline(null);
	}

	@Override
	public void printNewline(final ProgramUnitElement element) {
		final String line = createIndentation() + lineBuffer.toString();

		if (element == null) {
			printWriter.println(line);
		} else {
			final CompilationUnit compilationUnit = element.getCompilationUnit();

			assureLinesHolder(compilationUnit);

			final int sourceLineNumber = getSourceLineNumber(element);

			if (lastSourceLineNumber != null) {
				printFillerLinesBefore(sourceLineNumber, compilationUnit);
			}

			final String comment = createComment(sourceLineNumber,
					linesHolders.get(compilationUnit).getLine(sourceLineNumber));
			final String lineWithComment = StringUtils.rightPad(line, LINE_LENGTH) + comment;

			printWriter.println(lineWithComment);

			lastSourceLineNumber = sourceLineNumber;
		}

		lineBuffer.setLength(0);
	}

	@Override
	public void unindent() {
		indentation = Math.max(indentation - 1, 0);
	}
}
