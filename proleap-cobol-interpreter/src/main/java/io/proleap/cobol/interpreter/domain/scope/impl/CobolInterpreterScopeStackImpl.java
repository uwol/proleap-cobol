package io.proleap.cobol.interpreter.domain.scope.impl;

import java.util.Stack;

import io.proleap.cobol.interpreter.domain.scope.CobolInterpreterScope;
import io.proleap.cobol.interpreter.domain.scope.CobolInterpreterScopeStack;

public class CobolInterpreterScopeStackImpl implements CobolInterpreterScopeStack {

	protected final Stack<CobolInterpreterScope> scopes = new Stack<>();

	@Override
	public Stack<CobolInterpreterScope> getScopes() {
		return scopes;
	}

	@Override
	public void haltAll() {
		for (final CobolInterpreterScope scope : scopes) {
			scope.setHalted(true);
		}
	}

	@Override
	public CobolInterpreterScope peek() {
		return scopes.peek();
	}

	@Override
	public void pop() {
		scopes.pop();
	}

	@Override
	public CobolInterpreterScope push() {
		return scopes.push(new CobolInterpreterScopeImpl());
	}
}
