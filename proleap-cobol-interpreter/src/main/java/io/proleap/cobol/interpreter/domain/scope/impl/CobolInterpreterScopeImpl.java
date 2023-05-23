package io.proleap.cobol.interpreter.domain.scope.impl;

import io.proleap.cobol.interpreter.domain.scope.CobolInterpreterScope;

public class CobolInterpreterScopeImpl implements CobolInterpreterScope {

	private boolean halted = false;

	@Override
	public boolean isHalted() {
		return halted;
	}

	@Override
	public void setHalted(final boolean halted) {
		this.halted = halted;
	}
}
