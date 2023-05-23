package io.proleap.cobol.interpreter.domain.scope;

import java.util.Stack;

public interface CobolInterpreterScopeStack {

	Stack<CobolInterpreterScope> getScopes();

	void haltAll();

	CobolInterpreterScope peek();

	void pop();

	CobolInterpreterScope push();
}
