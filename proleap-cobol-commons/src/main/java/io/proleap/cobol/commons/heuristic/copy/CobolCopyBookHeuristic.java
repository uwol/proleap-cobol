package io.proleap.cobol.commons.heuristic.copy;

public interface CobolCopyBookHeuristic {

	final static String INDICATOR_FIELD = "[ABCdD\\t\\-# ]";

	final static String TANDEM_IDENTIFICATION = INDICATOR_FIELD + "\\s*identification.*";

	final static String VARIABLE_IDENTIFICATION = ".{6}" + INDICATOR_FIELD + "\\s*identification.*";

	boolean determineIsCopyBook(String cobolText);
}
