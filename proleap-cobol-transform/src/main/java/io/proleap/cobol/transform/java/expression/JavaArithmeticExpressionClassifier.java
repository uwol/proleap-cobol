package io.proleap.cobol.transform.java.expression;

import io.proleap.cobol.asg.metamodel.valuestmt.relation.ArithmeticComparison;

public interface JavaArithmeticExpressionClassifier {

	public enum JavaArithmeticExpressionTypeEnum {
		COMPARISON_BETWEEN_GROUP_AND_STRING, COMPARISON_BETWEEN_STRING_AND_BLANK, DEFAULT
	}

	JavaArithmeticExpressionTypeEnum classify(ArithmeticComparison arithmeticComparison);
}
