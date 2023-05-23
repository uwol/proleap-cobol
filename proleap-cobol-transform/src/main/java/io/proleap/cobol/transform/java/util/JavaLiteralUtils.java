package io.proleap.cobol.transform.java.util;

import java.math.BigDecimal;

public class JavaLiteralUtils {

	public static String mapToLiteral(final BigDecimal value) {
		if (value == null) {
			return "null";
		} else if (BigDecimal.ZERO.equals(value)) {
			return "BigDecimal.ZERO";
		} else if (BigDecimal.ONE.equals(value)) {
			return "BigDecimal.ONE";
		} else if (BigDecimal.TEN.equals(value)) {
			return "BigDecimal.TEN";
		} else {
			return String.format("BigDecimal.valueOf(%s)", value);
		}
	}

	public static String mapToLiteral(final Boolean value) {
		return String.valueOf(value);
	}

	public static String mapToLiteral(final String value) {
		return String.format("\"%s\"", value);
	}
}
