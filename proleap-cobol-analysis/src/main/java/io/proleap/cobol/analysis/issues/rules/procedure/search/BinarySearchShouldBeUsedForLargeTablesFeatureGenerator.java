package io.proleap.cobol.analysis.issues.rules.procedure.search;

import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.inject.Singleton;

import org.antlr.v4.runtime.tree.TerminalNode;

import io.proleap.cobol.CobolParser.SearchStatementContext;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.IntegerLiteral;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.OccursClause;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.search.SearchStatement;
import io.proleap.cobol.asg.metamodel.valuestmt.IntegerLiteralValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class BinarySearchShouldBeUsedForLargeTablesFeatureGenerator extends FeatureGenerator<Statement> {

	final static BigDecimal MAX_TABLE_ENTRIES = new BigDecimal(500);

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isLargeTable(final SearchStatement searchStatement) {
		final boolean result;
		final Call dataCall = searchStatement.getDataCall();

		if (dataCall == null) {
			result = false;
		} else {
			final DataDescriptionEntry dataDescriptionEntry = cobolDataDescriptionEntryService
					.getDataDescriptionEntry(dataCall);
			final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry
					.getDataDescriptionEntryType();

			if (DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)
					|| DataDescriptionEntryType.SCALAR.equals(dataDescriptionEntryType)) {
				final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
				final List<OccursClause> occursClauses = dataDescriptionEntryGroup.getOccursClauses();

				if (occursClauses == null) {
					result = false;
				} else {
					final OccursClause occursClause = occursClauses.get(0);

					if (occursClause == null) {
						result = false;
					} else {
						final ValueStmt fromValueStmt = occursClause.getFrom();

						if (fromValueStmt instanceof IntegerLiteralValueStmt) {
							final IntegerLiteralValueStmt fromIntegerLiteralValueStmt = (IntegerLiteralValueStmt) fromValueStmt;
							final IntegerLiteral from = fromIntegerLiteralValueStmt.getLiteral();
							final BigDecimal value = from.getValue();

							if (MAX_TABLE_ENTRIES.compareTo(value) >= 0) {
								result = false;
							} else {
								result = true;
							}
						} else {
							result = false;
						}
					}
				}
			} else {
				result = false;
			}
		}

		return result;
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final StatementType statementType = statement.getStatementType();
		final boolean result;

		if (!StatementTypeEnum.SEARCH.equals(statementType)) {
			result = false;
		} else {
			final SearchStatement searchStatement = (SearchStatement) statement;
			final SearchStatementContext searchStatementContext = (SearchStatementContext) searchStatement.getCtx();
			final TerminalNode all = searchStatementContext.ALL();

			if (all != null) {
				result = false;
			} else {
				result = isLargeTable(searchStatement);
			}
		}

		return result;
	}
}
