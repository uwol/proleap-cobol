package io.proleap.cobol.commons.stream;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.proleap.cobol.asg.metamodel.procedure.AtEndPhrase;
import io.proleap.cobol.asg.metamodel.procedure.InvalidKeyPhrase;
import io.proleap.cobol.asg.metamodel.procedure.NotAtEndPhrase;
import io.proleap.cobol.asg.metamodel.procedure.NotInvalidKeyPhrase;
import io.proleap.cobol.asg.metamodel.procedure.NotOnExceptionClause;
import io.proleap.cobol.asg.metamodel.procedure.NotOnOverflowPhrase;
import io.proleap.cobol.asg.metamodel.procedure.NotOnSizeErrorPhrase;
import io.proleap.cobol.asg.metamodel.procedure.OnExceptionClause;
import io.proleap.cobol.asg.metamodel.procedure.OnOverflowPhrase;
import io.proleap.cobol.asg.metamodel.procedure.OnSizeErrorPhrase;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.accept.AcceptStatement;
import io.proleap.cobol.asg.metamodel.procedure.add.AddStatement;
import io.proleap.cobol.asg.metamodel.procedure.call.CallStatement;
import io.proleap.cobol.asg.metamodel.procedure.compute.ComputeStatement;
import io.proleap.cobol.asg.metamodel.procedure.delete.DeleteStatement;
import io.proleap.cobol.asg.metamodel.procedure.divide.DivideStatement;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.EvaluateStatement;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.WhenOther;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.WhenPhrase;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.Else;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.IfStatement;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.Then;
import io.proleap.cobol.asg.metamodel.procedure.multiply.MultiplyStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformInlineStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement.PerformStatementType;
import io.proleap.cobol.asg.metamodel.procedure.read.ReadStatement;
import io.proleap.cobol.asg.metamodel.procedure.receive.ReceiveStatement;
import io.proleap.cobol.asg.metamodel.procedure.returnstmt.ReturnStatement;
import io.proleap.cobol.asg.metamodel.procedure.rewrite.RewriteStatement;
import io.proleap.cobol.asg.metamodel.procedure.search.SearchStatement;
import io.proleap.cobol.asg.metamodel.procedure.start.StartStatement;
import io.proleap.cobol.asg.metamodel.procedure.string.StringStatement;
import io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractStatement;
import io.proleap.cobol.asg.metamodel.procedure.unstring.UnstringStatement;
import io.proleap.cobol.asg.metamodel.procedure.write.AtEndOfPagePhrase;
import io.proleap.cobol.asg.metamodel.procedure.write.NotAtEndOfPagePhrase;
import io.proleap.cobol.asg.metamodel.procedure.write.WriteStatement;

public class CobolNestedStreams {

	protected static Stream<Statement> nestedStatements(final AcceptStatement acceptStatement) {
		final Stream<Statement> onExceptionStream;
		final Stream<Statement> notOnExceptionStream;

		final OnExceptionClause onExceptionClause = acceptStatement.getOnExceptionClause();
		final NotOnExceptionClause notOnExceptionClause = acceptStatement.getNotOnExceptionClause();

		if (onExceptionClause != null) {
			onExceptionStream = nestedStatements(onExceptionClause);
		} else {
			onExceptionStream = Stream.empty();
		}

		if (notOnExceptionClause != null) {
			notOnExceptionStream = nestedStatements(notOnExceptionClause);
		} else {
			notOnExceptionStream = Stream.empty();
		}

		return Stream.concat(onExceptionStream, notOnExceptionStream);
	}

	protected static Stream<Statement> nestedStatements(final AddStatement addStatement) {
		final Stream<Statement> onSizeErrorStream;
		final Stream<Statement> notOnSizeErrorStream;

		final OnSizeErrorPhrase onSizeErrorPhrase = addStatement.getOnSizeErrorPhrase();
		final NotOnSizeErrorPhrase notOnSizeErrorPhrase = addStatement.getNotOnSizeErrorPhrase();

		if (onSizeErrorPhrase != null) {
			onSizeErrorStream = nestedStatements(onSizeErrorPhrase);
		} else {
			onSizeErrorStream = Stream.empty();
		}

		if (notOnSizeErrorPhrase != null) {
			notOnSizeErrorStream = nestedStatements(notOnSizeErrorPhrase);
		} else {
			notOnSizeErrorStream = Stream.empty();
		}

		return Stream.concat(onSizeErrorStream, notOnSizeErrorStream);
	}

	protected static Stream<Statement> nestedStatements(final AtEndOfPagePhrase atEndOfPagePhrase) {
		return atEndOfPagePhrase.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final AtEndPhrase atEnd) {
		return atEnd.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final CallStatement callStatement) {
		final Stream<Statement> onExceptionStream;
		final Stream<Statement> notOnExceptionStream;
		final Stream<Statement> onOverflowStream;

		final OnOverflowPhrase onOverflowPhrase = callStatement.getOnOverflowPhrase();
		final OnExceptionClause onExceptionClause = callStatement.getOnExceptionClause();
		final NotOnExceptionClause notOnExceptionClause = callStatement.getNotOnExceptionClause();

		if (onExceptionClause != null) {
			onExceptionStream = nestedStatements(onExceptionClause);
		} else {
			onExceptionStream = Stream.empty();
		}

		if (notOnExceptionClause != null) {
			notOnExceptionStream = nestedStatements(notOnExceptionClause);
		} else {
			notOnExceptionStream = Stream.empty();
		}

		if (onOverflowPhrase != null) {
			onOverflowStream = nestedStatements(onOverflowPhrase);
		} else {
			onOverflowStream = Stream.empty();
		}

		return Stream.concat(onOverflowStream, Stream.concat(onExceptionStream, notOnExceptionStream));
	}

	protected static Stream<Statement> nestedStatements(final ComputeStatement computeStatement) {
		final Stream<Statement> onSizeErrorStream;
		final Stream<Statement> notOnSizeErrorStream;

		final OnSizeErrorPhrase onSizeErrorPhrase = computeStatement.getOnSizeErrorPhrase();
		final NotOnSizeErrorPhrase notOnSizeErrorPhrase = computeStatement.getNotOnSizeErrorPhrase();

		if (onSizeErrorPhrase != null) {
			onSizeErrorStream = nestedStatements(onSizeErrorPhrase);
		} else {
			onSizeErrorStream = Stream.empty();
		}

		if (notOnSizeErrorPhrase != null) {
			notOnSizeErrorStream = nestedStatements(notOnSizeErrorPhrase);
		} else {
			notOnSizeErrorStream = Stream.empty();
		}

		return Stream.concat(onSizeErrorStream, notOnSizeErrorStream);
	}

	protected static Stream<Statement> nestedStatements(final DeleteStatement deleteStatement) {
		final Stream<Statement> invalidKeyPhraseStream;
		final Stream<Statement> notInvalidKeyPhraseStream;

		final InvalidKeyPhrase invalidKeyPhrase = deleteStatement.getInvalidKeyPhrase();
		final NotInvalidKeyPhrase notInvalidKeyPhrase = deleteStatement.getNotInvalidKeyPhrase();

		if (invalidKeyPhrase != null) {
			invalidKeyPhraseStream = nestedStatements(invalidKeyPhrase);
		} else {
			invalidKeyPhraseStream = Stream.empty();
		}

		if (notInvalidKeyPhrase != null) {
			notInvalidKeyPhraseStream = nestedStatements(notInvalidKeyPhrase);
		} else {
			notInvalidKeyPhraseStream = Stream.empty();
		}

		return Stream.concat(invalidKeyPhraseStream, notInvalidKeyPhraseStream);
	}

	protected static Stream<Statement> nestedStatements(final DivideStatement divideStatement) {
		final Stream<Statement> onSizeErrorStream;
		final Stream<Statement> notOnSizeErrorStream;

		final OnSizeErrorPhrase onSizeErrorPhrase = divideStatement.getOnSizeErrorPhrase();
		final NotOnSizeErrorPhrase notOnSizeErrorPhrase = divideStatement.getNotOnSizeErrorPhrase();

		if (onSizeErrorPhrase != null) {
			onSizeErrorStream = nestedStatements(onSizeErrorPhrase);
		} else {
			onSizeErrorStream = Stream.empty();
		}

		if (notOnSizeErrorPhrase != null) {
			notOnSizeErrorStream = nestedStatements(notOnSizeErrorPhrase);
		} else {
			notOnSizeErrorStream = Stream.empty();
		}

		return Stream.concat(onSizeErrorStream, notOnSizeErrorStream);
	}

	protected static Stream<Statement> nestedStatements(final Else else1) {
		return else1.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final EvaluateStatement evaluateStatement) {
		final Stream<Statement> whenOtherStream;
		final Stream<Statement> whenPhrasesStream;

		final List<WhenPhrase> whenPhrases = evaluateStatement.getWhenPhrases();
		final WhenOther whenOther = evaluateStatement.getWhenOther();

		if (whenPhrases != null) {
			final List<Statement> whenPhrasesStatements = new ArrayList<>();

			for (final WhenPhrase whenPhrase : whenPhrases) {
				whenPhrasesStatements.addAll(whenPhrase.getStatements());
			}

			whenPhrasesStream = whenPhrasesStatements.stream();
		} else {
			whenPhrasesStream = Stream.empty();
		}

		if (whenOther != null) {
			whenOtherStream = nestedStatements(whenOther);
		} else {
			whenOtherStream = Stream.empty();
		}

		return Stream.concat(whenPhrasesStream, whenOtherStream);
	}

	protected static Stream<Statement> nestedStatements(final IfStatement ifStatement) {
		final Stream<Statement> thenStream;
		final Stream<Statement> else1Stream;

		final Then then = ifStatement.getThen();
		final Else else1 = ifStatement.getElse();

		if (then != null) {
			thenStream = nestedStatements(then);
		} else {
			thenStream = Stream.empty();
		}

		if (else1 != null) {
			else1Stream = nestedStatements(else1);
		} else {
			else1Stream = Stream.empty();
		}

		return Stream.concat(thenStream, else1Stream);
	}

	protected static Stream<Statement> nestedStatements(final InvalidKeyPhrase invalidKeyPhrase) {
		return invalidKeyPhrase.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final MultiplyStatement multiplyStatement) {
		final Stream<Statement> onSizeErrorStream;
		final Stream<Statement> notOnSizeErrorStream;

		final OnSizeErrorPhrase onSizeErrorPhrase = multiplyStatement.getOnSizeErrorPhrase();
		final NotOnSizeErrorPhrase notOnSizeErrorPhrase = multiplyStatement.getNotOnSizeErrorPhrase();

		if (onSizeErrorPhrase != null) {
			onSizeErrorStream = nestedStatements(onSizeErrorPhrase);
		} else {
			onSizeErrorStream = Stream.empty();
		}

		if (notOnSizeErrorPhrase != null) {
			notOnSizeErrorStream = nestedStatements(notOnSizeErrorPhrase);
		} else {
			notOnSizeErrorStream = Stream.empty();
		}

		return Stream.concat(onSizeErrorStream, notOnSizeErrorStream);
	}

	protected static Stream<Statement> nestedStatements(final NotAtEndOfPagePhrase notAtEndOfPagePhrase) {
		return notAtEndOfPagePhrase.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final NotAtEndPhrase notAtEndPhrase) {
		return notAtEndPhrase.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final NotInvalidKeyPhrase notInvalidKeyPhrase) {
		return notInvalidKeyPhrase.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final NotOnExceptionClause notOnExceptionClause) {
		return notOnExceptionClause.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final NotOnOverflowPhrase notOnOverflowPhrase) {
		return notOnOverflowPhrase.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final NotOnSizeErrorPhrase notOnSizeErrorPhrase) {
		return notOnSizeErrorPhrase.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final OnExceptionClause onExceptionClause) {
		return onExceptionClause.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final OnOverflowPhrase onOverflowPhrase) {
		return onOverflowPhrase.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final OnSizeErrorPhrase onSizeErrorPhrase) {
		return onSizeErrorPhrase.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final PerformInlineStatement performInlineStatement) {
		return performInlineStatement.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final PerformStatement performStatement) {
		final Stream<Statement> inlineStream;
		final PerformStatementType performStatementType = performStatement.getPerformStatementType();

		if (PerformStatement.PerformStatementType.INLINE.equals(performStatementType)) {
			final PerformInlineStatement performInlineStatement = performStatement.getPerformInlineStatement();
			inlineStream = nestedStatements(performInlineStatement);
		} else {
			inlineStream = Stream.empty();
		}

		return inlineStream;
	}

	protected static Stream<Statement> nestedStatements(final ReadStatement readStatement) {
		final Stream<Statement> invalidKeyPhraseStream;
		final Stream<Statement> notInvalidKeyPhraseStream;
		final Stream<Statement> atEndStream;
		final Stream<Statement> notAtEndPhraseStream;

		final InvalidKeyPhrase invalidKeyPhrase = readStatement.getInvalidKeyPhrase();
		final NotInvalidKeyPhrase notInvalidKeyPhrase = readStatement.getNotInvalidKeyPhrase();
		final AtEndPhrase atEnd = readStatement.getAtEnd();
		final NotAtEndPhrase notAtEndPhrase = readStatement.getNotAtEndPhrase();

		if (invalidKeyPhrase != null) {
			invalidKeyPhraseStream = nestedStatements(invalidKeyPhrase);
		} else {
			invalidKeyPhraseStream = Stream.empty();
		}

		if (notInvalidKeyPhrase != null) {
			notInvalidKeyPhraseStream = nestedStatements(notInvalidKeyPhrase);
		} else {
			notInvalidKeyPhraseStream = Stream.empty();
		}

		if (atEnd != null) {
			atEndStream = nestedStatements(atEnd);
		} else {
			atEndStream = Stream.empty();
		}

		if (notAtEndPhrase != null) {
			notAtEndPhraseStream = nestedStatements(notAtEndPhrase);
		} else {
			notAtEndPhraseStream = Stream.empty();
		}

		return Stream.concat(Stream.concat(invalidKeyPhraseStream, notInvalidKeyPhraseStream),
				Stream.concat(atEndStream, notAtEndPhraseStream));
	}

	protected static Stream<Statement> nestedStatements(final ReceiveStatement receiveStatement) {
		final Stream<Statement> onExceptionStream;
		final Stream<Statement> notOnExceptionStream;

		final OnExceptionClause onExceptionClause = receiveStatement.getOnExceptionClause();
		final NotOnExceptionClause notOnExceptionClause = receiveStatement.getNotOnExceptionClause();

		if (onExceptionClause != null) {
			onExceptionStream = nestedStatements(onExceptionClause);
		} else {
			onExceptionStream = Stream.empty();
		}

		if (notOnExceptionClause != null) {
			notOnExceptionStream = nestedStatements(notOnExceptionClause);
		} else {
			notOnExceptionStream = Stream.empty();
		}

		return Stream.concat(onExceptionStream, notOnExceptionStream);
	}

	protected static Stream<Statement> nestedStatements(final ReturnStatement returnStatement) {
		final Stream<Statement> atEndStream;
		final Stream<Statement> notAtEndPhraseStream;

		final AtEndPhrase atEnd = returnStatement.getAtEndPhrase();
		final NotAtEndPhrase notAtEndPhrase = returnStatement.getNotAtEndPhrase();

		if (atEnd != null) {
			atEndStream = nestedStatements(atEnd);
		} else {
			atEndStream = Stream.empty();
		}

		if (notAtEndPhrase != null) {
			notAtEndPhraseStream = nestedStatements(notAtEndPhrase);
		} else {
			notAtEndPhraseStream = Stream.empty();
		}

		return Stream.concat(atEndStream, notAtEndPhraseStream);
	}

	protected static Stream<Statement> nestedStatements(final RewriteStatement rewriteStatement) {
		final Stream<Statement> invalidKeyPhraseStream;
		final Stream<Statement> notInvalidKeyPhraseStream;

		final InvalidKeyPhrase invalidKeyPhrase = rewriteStatement.getInvalidKeyPhrase();
		final NotInvalidKeyPhrase notInvalidKeyPhrase = rewriteStatement.getNotInvalidKeyPhrase();

		if (invalidKeyPhrase != null) {
			invalidKeyPhraseStream = nestedStatements(invalidKeyPhrase);
		} else {
			invalidKeyPhraseStream = Stream.empty();
		}

		if (notInvalidKeyPhrase != null) {
			notInvalidKeyPhraseStream = nestedStatements(notInvalidKeyPhrase);
		} else {
			notInvalidKeyPhraseStream = Stream.empty();
		}

		return Stream.concat(invalidKeyPhraseStream, notInvalidKeyPhraseStream);
	}

	protected static Stream<Statement> nestedStatements(final SearchStatement searchStatement) {
		final Stream<Statement> atEndStream;
		final Stream<Statement> searchWhenStream;

		final AtEndPhrase atEnd = searchStatement.getAtEndPhrase();
		final List<io.proleap.cobol.asg.metamodel.procedure.search.WhenPhrase> whenPhrases = searchStatement
				.getWhenPhrases();

		if (atEnd != null) {
			atEndStream = nestedStatements(atEnd);
		} else {
			atEndStream = Stream.empty();
		}

		if (whenPhrases != null) {
			final List<Statement> whenStatements = new ArrayList<>();

			for (final io.proleap.cobol.asg.metamodel.procedure.search.WhenPhrase whenPhrase : whenPhrases) {
				whenStatements.addAll(whenPhrase.getStatements());
			}

			searchWhenStream = whenStatements.stream();
		} else {
			searchWhenStream = Stream.empty();
		}

		return Stream.concat(atEndStream, searchWhenStream);
	}

	protected static Stream<Statement> nestedStatements(final StartStatement startStatement) {
		final Stream<Statement> invalidKeyPhraseStream;
		final Stream<Statement> notInvalidKeyPhraseStream;

		final InvalidKeyPhrase invalidKeyPhrase = startStatement.getInvalidKeyPhrase();
		final NotInvalidKeyPhrase notInvalidKeyPhrase = startStatement.getNotInvalidKeyPhrase();

		if (invalidKeyPhrase != null) {
			invalidKeyPhraseStream = nestedStatements(invalidKeyPhrase);
		} else {
			invalidKeyPhraseStream = Stream.empty();
		}

		if (notInvalidKeyPhrase != null) {
			notInvalidKeyPhraseStream = nestedStatements(notInvalidKeyPhrase);
		} else {
			notInvalidKeyPhraseStream = Stream.empty();
		}

		return Stream.concat(invalidKeyPhraseStream, notInvalidKeyPhraseStream);
	}

	public static Stream<Statement> nestedStatements(final Statement statement) {
		final Stream<Statement> result;
		final StatementTypeEnum statementType = (StatementTypeEnum) statement.getStatementType();

		switch (statementType) {
		case ACCEPT:
			final AcceptStatement acceptStatement = (AcceptStatement) statement;
			result = nestedStatements(acceptStatement);
			break;
		case ADD:
			final AddStatement addStatement = (AddStatement) statement;
			result = nestedStatements(addStatement);
			break;
		case CALL:
			final CallStatement callStatement = (CallStatement) statement;
			result = nestedStatements(callStatement);
			break;
		case COMPUTE:
			final ComputeStatement computeStatement = (ComputeStatement) statement;
			result = nestedStatements(computeStatement);
			break;
		case DELETE:
			final DeleteStatement deleteStatement = (DeleteStatement) statement;
			result = nestedStatements(deleteStatement);
			break;
		case DIVIDE:
			final DivideStatement divideStatement = (DivideStatement) statement;
			result = nestedStatements(divideStatement);
			break;
		case EVALUATE:
			final EvaluateStatement evaluateStatement = (EvaluateStatement) statement;
			result = nestedStatements(evaluateStatement);
			break;
		case IF:
			final IfStatement ifStatement = (IfStatement) statement;
			result = nestedStatements(ifStatement);
			break;
		case MULTIPLY:
			final MultiplyStatement multiplyStatement = (MultiplyStatement) statement;
			result = nestedStatements(multiplyStatement);
			break;
		case PERFORM:
			final PerformStatement performStatement = (PerformStatement) statement;
			result = nestedStatements(performStatement);
			break;
		case READ:
			final ReadStatement readStatement = (ReadStatement) statement;
			result = nestedStatements(readStatement);
			break;
		case RECEIVE:
			final ReceiveStatement receiveStatement = (ReceiveStatement) statement;
			result = nestedStatements(receiveStatement);
			break;
		case RETURN:
			final ReturnStatement returnStatement = (ReturnStatement) statement;
			result = nestedStatements(returnStatement);
			break;
		case REWRITE:
			final RewriteStatement rewriteStatement = (RewriteStatement) statement;
			result = nestedStatements(rewriteStatement);
			break;
		case SEARCH:
			final SearchStatement searchStatement = (SearchStatement) statement;
			result = nestedStatements(searchStatement);
			break;
		case START:
			final StartStatement startStatement = (StartStatement) statement;
			result = nestedStatements(startStatement);
			break;
		case STRING:
			final StringStatement stringStatement = (StringStatement) statement;
			result = nestedStatements(stringStatement);
			break;
		case SUBTRACT:
			final SubtractStatement subtractStatement = (SubtractStatement) statement;
			result = nestedStatements(subtractStatement);
			break;
		case UNSTRING:
			final UnstringStatement unstringStatement = (UnstringStatement) statement;
			result = nestedStatements(unstringStatement);
			break;
		case WRITE:
			final WriteStatement writeStatement = (WriteStatement) statement;
			result = nestedStatements(writeStatement);
			break;
		default:
			result = Stream.empty();
			break;
		}

		return result;
	}

	protected static Stream<Statement> nestedStatements(final StringStatement stringStatement) {
		final Stream<Statement> onOverflowStream;
		final Stream<Statement> notOnOverflowStream;

		final OnOverflowPhrase onOverflowPhrase = stringStatement.getOnOverflowPhrase();
		final NotOnOverflowPhrase notOnOverflowPhrase = stringStatement.getNotOnOverflowPhrase();

		if (onOverflowPhrase != null) {
			onOverflowStream = nestedStatements(onOverflowPhrase);
		} else {
			onOverflowStream = Stream.empty();
		}

		if (notOnOverflowPhrase != null) {
			notOnOverflowStream = nestedStatements(notOnOverflowPhrase);
		} else {
			notOnOverflowStream = Stream.empty();
		}

		return Stream.concat(onOverflowStream, notOnOverflowStream);
	}

	protected static Stream<Statement> nestedStatements(final SubtractStatement subtractStatement) {
		final Stream<Statement> onSizeErrorStream;
		final Stream<Statement> notOnSizeErrorStream;

		final OnSizeErrorPhrase onSizeErrorPhrase = subtractStatement.getOnSizeErrorPhrase();
		final NotOnSizeErrorPhrase notOnSizeErrorPhrase = subtractStatement.getNotOnSizeErrorPhrase();

		if (onSizeErrorPhrase != null) {
			onSizeErrorStream = nestedStatements(onSizeErrorPhrase);
		} else {
			onSizeErrorStream = Stream.empty();
		}

		if (notOnSizeErrorPhrase != null) {
			notOnSizeErrorStream = nestedStatements(notOnSizeErrorPhrase);
		} else {
			notOnSizeErrorStream = Stream.empty();
		}

		return Stream.concat(onSizeErrorStream, notOnSizeErrorStream);
	}

	protected static Stream<Statement> nestedStatements(final Then then) {
		return then.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final UnstringStatement unstringStatement) {
		final Stream<Statement> onOverflowStream;
		final Stream<Statement> notOnOverflowStream;

		final OnOverflowPhrase onOverflowPhrase = unstringStatement.getOnOverflowPhrase();
		final NotOnOverflowPhrase notOnOverflowPhrase = unstringStatement.getNotOnOverflowPhrase();

		if (onOverflowPhrase != null) {
			onOverflowStream = nestedStatements(onOverflowPhrase);
		} else {
			onOverflowStream = Stream.empty();
		}

		if (notOnOverflowPhrase != null) {
			notOnOverflowStream = nestedStatements(notOnOverflowPhrase);
		} else {
			notOnOverflowStream = Stream.empty();
		}

		return Stream.concat(onOverflowStream, notOnOverflowStream);
	}

	protected static Stream<Statement> nestedStatements(final WhenOther whenOther) {
		return whenOther.getStatements().stream();
	}

	protected static Stream<Statement> nestedStatements(final WriteStatement writeStatement) {
		final Stream<Statement> invalidKeyPhraseStream;
		final Stream<Statement> notInvalidKeyPhraseStream;
		final Stream<Statement> atEndOfPagePhraseStream;
		final Stream<Statement> notAtEndOfPagePhraseStream;

		final InvalidKeyPhrase invalidKeyPhrase = writeStatement.getInvalidKeyPhrase();
		final NotInvalidKeyPhrase notInvalidKeyPhrase = writeStatement.getNotInvalidKeyPhrase();
		final AtEndOfPagePhrase atEndOfPagePhrase = writeStatement.getAtEndOfPagePhrase();
		final NotAtEndOfPagePhrase notAtEndOfPagePhrase = writeStatement.getNotAtEndOfPagePhrase();

		if (invalidKeyPhrase != null) {
			invalidKeyPhraseStream = nestedStatements(invalidKeyPhrase);
		} else {
			invalidKeyPhraseStream = Stream.empty();
		}

		if (notInvalidKeyPhrase != null) {
			notInvalidKeyPhraseStream = nestedStatements(notInvalidKeyPhrase);
		} else {
			notInvalidKeyPhraseStream = Stream.empty();
		}

		if (atEndOfPagePhrase != null) {
			atEndOfPagePhraseStream = nestedStatements(atEndOfPagePhrase);
		} else {
			atEndOfPagePhraseStream = Stream.empty();
		}

		if (notAtEndOfPagePhrase != null) {
			notAtEndOfPagePhraseStream = nestedStatements(notAtEndOfPagePhrase);
		} else {
			notAtEndOfPagePhraseStream = Stream.empty();
		}

		return Stream.concat(Stream.concat(atEndOfPagePhraseStream, notAtEndOfPagePhraseStream),
				Stream.concat(invalidKeyPhraseStream, notInvalidKeyPhraseStream));
	}

	public static Stream<Statement> nestedStatementsRec(final Statement statement) {
		final List<Statement> statements = nestedStatements(statement).collect(Collectors.toList());
		if (!statements.isEmpty()) {
			return Stream.concat(statements.stream(),
					statements.stream().flatMap(CobolNestedStreams::nestedStatementsRec));
		}
		return statements.stream();
	}
}
