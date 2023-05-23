package io.proleap.cobol.transform.printer;

import io.proleap.cobol.asg.metamodel.ProgramUnitElement;

public interface Printer {

	void close();

	void flush();

	void indent();

	void print(String str);

	void print(String format, Object... args);

	void printNewline();

	void printNewline(ProgramUnitElement element);

	void unindent();

}
