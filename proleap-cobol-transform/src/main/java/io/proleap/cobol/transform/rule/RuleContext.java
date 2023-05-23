package io.proleap.cobol.transform.rule;

import org.antlr.v4.runtime.tree.ParseTree;

import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnitElement;
import io.proleap.cobol.transform.java.printer.TypedPrinter;
import io.proleap.cobol.transform.printer.Printer;

public class RuleContext {

	protected String packageName;

	protected Printer printer;

	protected Program program;

	protected CobolTransformRuleMatcher ruleMatcher;

	protected TypedPrinter typedPrinter;

	public String getPackageName() {
		return packageName;
	}

	public Printer getPrinter() {
		return printer;
	}

	public Program getProgram() {
		return program;
	}

	public CobolTransformRuleMatcher getRuleMatcher() {
		return ruleMatcher;
	}

	public TypedPrinter getTypedPrinter() {
		return typedPrinter;
	}

	public void p(final String str) {
		printer.print(str);
	}

	public void p(final String format, final Object... args) {
		printer.print(format, args);
	}

	public void pNl() {
		printer.printNewline();
	}

	public void pNl(final ProgramUnitElement programUnitElement) {
		printer.printNewline(programUnitElement);
	}

	public void setPackageName(final String packageName) {
		this.packageName = packageName;
	}

	public void setPrinter(final Printer printer) {
		this.printer = printer;
	}

	public void setProgram(final Program program) {
		this.program = program;
	}

	public void setRuleMatcher(final CobolTransformRuleMatcher ruleMatcher) {
		this.ruleMatcher = ruleMatcher;
	}

	public void setTypedPrinter(final TypedPrinter typedPrinter) {
		this.typedPrinter = typedPrinter;
	}

	public void visit(final ParseTree ctx) {
		ruleMatcher.apply(ctx, this);
	}

	public void visitChildren(final ParseTree ctx) {
		ruleMatcher.applyOnChildren(ctx, this);
	}
}
