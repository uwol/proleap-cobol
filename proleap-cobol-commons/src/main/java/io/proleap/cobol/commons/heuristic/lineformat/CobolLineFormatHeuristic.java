package io.proleap.cobol.commons.heuristic.lineformat;

import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

public interface CobolLineFormatHeuristic {

	final static String INDICATOR_FIELD = "[ABCdD\\t\\-/*# ]";

	final static String SOURCE_FORMAT_FIXED = ".{6}(" + INDICATOR_FIELD + ".{65}.{8})?";

	final static String SOURCE_FORMAT_TANDEM_IDENTIFICATION = INDICATOR_FIELD + "\\s*identification.*";

	final static String SOURCE_FORMAT_VARIABLE_IDENTIFICATION = ".{6}" + INDICATOR_FIELD + "\\s*identification.*";

	CobolSourceFormatEnum determineLineFormat(String cobolText);
}
