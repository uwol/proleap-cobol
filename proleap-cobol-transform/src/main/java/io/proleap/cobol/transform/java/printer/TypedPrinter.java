package io.proleap.cobol.transform.java.printer;

import org.antlr.v4.runtime.tree.ParseTree;

import io.proleap.cobol.commons.type.CobolTypeEnum;

public interface TypedPrinter {

	void printWithAdjustedType(ParseTree ctx, CobolTypeEnum originalType, CobolTypeEnum typeToConvertTo);

	void printWithAdjustedType(ParseTree ctx, String suffix, CobolTypeEnum originalType, CobolTypeEnum typeToConvertTo);
}
