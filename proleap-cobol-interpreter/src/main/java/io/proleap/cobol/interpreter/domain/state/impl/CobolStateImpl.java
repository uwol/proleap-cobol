package io.proleap.cobol.interpreter.domain.state.impl;

import java.util.HashMap;
import java.util.Map;

import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.interpreter.domain.console.CobolConsole;
import io.proleap.cobol.interpreter.domain.console.impl.CobolConsoleImpl;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.domain.scope.CobolInterpreterScopeStack;
import io.proleap.cobol.interpreter.domain.scope.impl.CobolInterpreterScopeStackImpl;
import io.proleap.cobol.interpreter.domain.state.CobolState;
import io.proleap.cobol.interpreter.domain.storage.CobolStorageArea;
import io.proleap.cobol.interpreter.domain.storage.impl.CobolStorageAreaImpl;

public class CobolStateImpl implements CobolState {

	protected final CobolConsole console = new CobolConsoleImpl();

	protected final Map<FileControlEntry, CobolFile> files = new HashMap<>();

	protected boolean halted = false;

	protected int ops = 0;

	protected final CobolInterpreterScopeStack performScopes = new CobolInterpreterScopeStackImpl();

	protected final CobolInterpreterScopeStack statementsHandlerScopes = new CobolInterpreterScopeStackImpl();

	protected final CobolStorageArea storageArea = new CobolStorageAreaImpl();

	@Override
	public CobolConsole getConsole() {
		return console;
	}

	@Override
	public CobolFile getFile(final FileControlEntry key) {
		return files.get(key);
	}

	@Override
	public Map<FileControlEntry, CobolFile> getFiles() {
		return files;
	}

	@Override
	public int getOps() {
		return ops;
	}

	@Override
	public CobolInterpreterScopeStack getPerformScopes() {
		return performScopes;
	}

	@Override
	public CobolInterpreterScopeStack getStatementsHandlerScopes() {
		return statementsHandlerScopes;
	}

	@Override
	public CobolStorageArea getStorage() {
		return storageArea;
	}

	@Override
	public void incOps() {
		ops++;
	}

	@Override
	public void incOps(final int ops) {
		this.ops += ops;
	}

	@Override
	public boolean isHalted() {
		return halted;
	}

	@Override
	public void putFile(final FileControlEntry key, final CobolFile file) {
		files.put(key, file);
	}

	@Override
	public void setHalted(final boolean halted) {
		this.halted = halted;
	}
}
