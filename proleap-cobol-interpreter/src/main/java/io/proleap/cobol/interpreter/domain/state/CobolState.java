package io.proleap.cobol.interpreter.domain.state;

import java.util.Map;

import io.proleap.cobol.asg.metamodel.environment.inputoutput.filecontrol.FileControlEntry;
import io.proleap.cobol.interpreter.domain.console.CobolConsole;
import io.proleap.cobol.interpreter.domain.file.CobolFile;
import io.proleap.cobol.interpreter.domain.scope.CobolInterpreterScopeStack;
import io.proleap.cobol.interpreter.domain.storage.CobolStorageArea;

public interface CobolState {

	CobolConsole getConsole();

	CobolFile getFile(FileControlEntry key);

	Map<FileControlEntry, CobolFile> getFiles();

	int getOps();

	CobolInterpreterScopeStack getPerformScopes();

	CobolInterpreterScopeStack getStatementsHandlerScopes();

	CobolStorageArea getStorage();

	void incOps();

	void incOps(int ops);

	boolean isHalted();

	void putFile(FileControlEntry key, CobolFile entity);

	void setHalted(boolean halted);
}
