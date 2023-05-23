package io.proleap.cobol.analysis.util;

import java.util.List;

import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;

public class TokenUtils {

	public static String getHiddenTokensToLeft(final int tokPos, final BufferedTokenStream tokens) {
		final List<Token> refChannel = tokens.getHiddenTokensToLeft(tokPos, Lexer.HIDDEN);
		final StringBuffer sb = new StringBuffer();

		if (refChannel != null) {
			for (final Token refToken : refChannel) {
				final String text = refToken.getText();
				sb.append(text);
			}
		}

		return sb.toString();
	}
}
