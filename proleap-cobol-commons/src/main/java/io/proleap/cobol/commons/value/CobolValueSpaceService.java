package io.proleap.cobol.commons.value;

import javax.inject.Singleton;

import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;

@Singleton
public interface CobolValueSpaceService {

	boolean isSpace(ValueStmt valueStmt);
}
