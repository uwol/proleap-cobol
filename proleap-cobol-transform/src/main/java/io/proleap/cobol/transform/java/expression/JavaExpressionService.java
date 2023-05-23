package io.proleap.cobol.transform.java.expression;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.DataDescriptionEntryCall;
import io.proleap.cobol.asg.metamodel.call.SpecialRegisterCall;
import io.proleap.cobol.asg.metamodel.call.TableCall;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.CallValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.ConditionValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Basis;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDiv;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDivs;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.PlusMinus;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Powers;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.AndOrCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.CombinableCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.ConditionNameReference;
import io.proleap.cobol.asg.metamodel.valuestmt.condition.SimpleCondition;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.ArithmeticComparison;

public interface JavaExpressionService {

	String mapToCall(Call call);

	String mapToCall(DataDescriptionEntryCall dataDescriptionEntryCall);

	String mapToCall(FileDescriptionEntry fileDescriptionEntry);

	String mapToCall(SpecialRegisterCall specialRegisterCall);

	String mapToCall(TableCall tableCall);

	String mapToExpression(AndOrCondition andOrCondition);

	String mapToExpression(ArithmeticComparison arithmeticComparison);

	String mapToExpression(ArithmeticValueStmt arithmeticValueStmt);

	String mapToExpression(Basis basis);

	String mapToExpression(CallValueStmt callValueStmt);

	String mapToExpression(CombinableCondition combinableCondition);

	String mapToExpression(ConditionNameReference conditionNameReference);

	String mapToExpression(ConditionValueStmt conditionValueStmt);

	String mapToExpression(MultDiv multDiv);

	String mapToExpression(MultDivs multDivs);

	String mapToExpression(PlusMinus plusMinus);

	String mapToExpression(Powers powers);

	String mapToExpression(RelationConditionValueStmt relationCondition);

	String mapToExpression(SimpleCondition simpleCondition);

	String mapToExpression(ValueStmt valueStmt);
}
