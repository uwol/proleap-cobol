package io.proleap.cobol.interpreter.domain.scope;

public interface CobolInterpreterScope {

	boolean isHalted();

	void setHalted(boolean halted);
}
