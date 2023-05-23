package io.proleap.cobol.analysis.issues.rules.procedure.call;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryCondition;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.ValueClause;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.call.ByContent;
import io.proleap.cobol.asg.metamodel.procedure.call.ByContentPhrase;
import io.proleap.cobol.asg.metamodel.procedure.call.ByReference;
import io.proleap.cobol.asg.metamodel.procedure.call.ByReferencePhrase;
import io.proleap.cobol.asg.metamodel.procedure.call.ByValue;
import io.proleap.cobol.asg.metamodel.procedure.call.ByValuePhrase;
import io.proleap.cobol.asg.metamodel.procedure.call.CallStatement;
import io.proleap.cobol.asg.metamodel.procedure.call.UsingParameter;
import io.proleap.cobol.asg.metamodel.procedure.call.UsingParameter.ParameterType;
import io.proleap.cobol.asg.metamodel.procedure.call.UsingPhrase;
import io.proleap.cobol.asg.metamodel.procedure.initialize.InitializeStatement;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveStatement;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveStatement.MoveType;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveToSendingArea;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveToStatement;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.util.CobolStreamUtils;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;
import io.proleap.cobol.commons.value.domain.CobolValue;

@Singleton
public class CallParametersShouldBeInitializedFeatureGenerator extends FeatureGenerator<Statement> {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Inject
	private CobolValueService valueService;

	@Inject
	private CobolValueStmtService valueStmtService;

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return (StatementTypeEnum.CALL.equals(statement.getStatementType()));
		}).filter(statement -> {
			final CallStatement callStatement = (CallStatement) statement;
			return isRelevantCallStatement(callStatement);
		});
	}

	protected boolean hasValueClause(final DataDescriptionEntry dataDescriptionEntry) {
		final boolean result;

		if (dataDescriptionEntry == null) {
			result = false;
		} else {
			final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry
					.getDataDescriptionEntryType();

			if (DataDescriptionEntryType.CONDITION.equals(dataDescriptionEntryType)) {
				final DataDescriptionEntryCondition dataDescriptionEntryCondition = (DataDescriptionEntryCondition) dataDescriptionEntry;
				final ValueClause valueClause = dataDescriptionEntryCondition.getValueClause();

				if (valueClause == null) {
					result = false;
				} else {
					result = true;
				}
			} else if (DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)
					|| DataDescriptionEntryType.SCALAR.equals(dataDescriptionEntryType)) {
				final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
				final ValueClause valueClause = dataDescriptionEntryGroup.getValueClause();

				if (valueClause == null) {
					result = false;
				} else {
					result = true;
				}
			} else {
				result = false;
			}
		}

		return result;
	}

	protected boolean isDefinedInLinkageSection(final DataDescriptionEntry dataDescriptionEntry) {
		final List<DataDescriptionEntry> linkageSectionDataDescriptionEntries = CobolStreamUtils
				.linkageSectionDataDescriptionEntries(dataDescriptionEntry.getCompilationUnit())
				.collect(Collectors.toList());
		return (linkageSectionDataDescriptionEntries.contains(dataDescriptionEntry));
	}

	protected boolean isInitializedDataItem(final DataDescriptionEntry dataDescriptionEntry) {
		final boolean result;

		if (dataDescriptionEntry == null) {
			result = false;
		} else {
			result = hasValueClause(dataDescriptionEntry) || parentInitialized(dataDescriptionEntry)
					|| moveSpacesOrInitializeUsed(dataDescriptionEntry)
					|| isDefinedInLinkageSection(dataDescriptionEntry);
		}

		return result;
	}

	protected boolean isRelevantCallStatement(final CallStatement callStatement) {
		final UsingPhrase usingPhrase = callStatement.getUsingPhrase();
		final boolean result;

		if (usingPhrase == null) {
			result = false;
		} else {
			final List<UsingParameter> usingParameters = usingPhrase.getUsingParameters();
			final List<ValueStmt> valueStmts = new ArrayList<ValueStmt>();

			for (final UsingParameter usingParameter : usingParameters) {
				final ParameterType parameterType = usingParameter.getParameterType();

				if (ParameterType.VALUE.equals(parameterType)) {
					final ByValuePhrase byValuePhrase = usingParameter.getByValuePhrase();
					final List<ByValue> byValues = byValuePhrase.getByValues();

					for (final ByValue byValue : byValues) {
						final ValueStmt valueStmt = byValue.getValueStmt();
						valueStmts.add(valueStmt);
					}
				} else if (ParameterType.REFERENCE.equals(parameterType)) {
					final ByReferencePhrase byReferencePhrase = usingParameter.getByReferencePhrase();
					final List<ByReference> byReferences = byReferencePhrase.getByReferences();

					for (final ByReference byReference : byReferences) {
						final ValueStmt valueStmt = byReference.getValueStmt();
						valueStmts.add(valueStmt);
					}
				} else {
					final ByContentPhrase byContentPhrase = usingParameter.getByContentPhrase();
					final List<ByContent> byContents = byContentPhrase.getByContents();

					for (final ByContent byContent : byContents) {
						final ValueStmt valueStmt = byContent.getValueStmt();
						valueStmts.add(valueStmt);
					}
				}
			}

			final List<DataDescriptionEntry> collectedDataDescriptionEntries = valueStmts.stream()
					.map(stmt -> cobolDataDescriptionEntryService.getDataDescriptionEntry(stmt))
					.collect(Collectors.toList());

			collectedDataDescriptionEntries
					.removeIf(dataDescriptionEntry -> isInitializedDataItem(dataDescriptionEntry));
			result = !collectedDataDescriptionEntries.isEmpty();
		}

		return result;
	}

	protected boolean moveSpacesOrInitializeUsed(final DataDescriptionEntry dataDescriptionEntry) {
		final ProgramUnit programUnit = dataDescriptionEntry.getProgramUnit();

		final List<Statement> collectedStatements = CobolStreamUtils
				.statementsRec(dataDescriptionEntry.getCompilationUnit()).collect(Collectors.toList());
		Iterator<Statement> iterator = collectedStatements.iterator();
		boolean dataItemMoved = false;
		boolean dataItemInitialized = false;

		while (!dataItemMoved & iterator.hasNext()) {
			final Statement nextStmt = iterator.next();
			final StatementType nextStatementType = nextStmt.getStatementType();

			if (StatementTypeEnum.MOVE.equals(nextStatementType)) {
				final MoveStatement moveStatement = (MoveStatement) nextStmt;
				final MoveType moveType = moveStatement.getMoveType();

				if (MoveType.MOVE_TO.equals(moveType)) {
					final MoveToStatement moveToStatement = moveStatement.getMoveToStatement();
					final MoveToSendingArea sendingArea = moveToStatement.getSendingArea();
					final ValueStmt sendingAreaValueStmt = sendingArea.getSendingAreaValueStmt();
					final CobolValue value = valueStmtService.getValue(sendingAreaValueStmt, null);

					if (CobolValueStmtService.WS.equals(valueService.getAsString(value, programUnit))) {
						final List<Call> receivingAreaCalls = moveToStatement.getReceivingAreaCalls();

						for (final Call call : receivingAreaCalls) {
							final DataDescriptionEntry calledDataDescriptionEntry = cobolDataDescriptionEntryService
									.getDataDescriptionEntry(call);

							if (calledDataDescriptionEntry != null) {
								if (cobolDataDescriptionEntryService.getDataDescriptionEntry(call)
										.equals(dataDescriptionEntry)) {
									dataItemMoved = true;
									break;
								}
							}
						}
					}
				}
			}
		}

		iterator = collectedStatements.iterator();

		while (!dataItemInitialized & iterator.hasNext()) {
			final Statement nextStmt = iterator.next();
			final StatementType nextStatementType = nextStmt.getStatementType();

			if (StatementTypeEnum.INITIALIZE.equals(nextStatementType)) {
				final InitializeStatement initializeStatement = (InitializeStatement) nextStmt;
				final List<Call> dataItemCalls = initializeStatement.getDataItemCalls();

				for (final Call call : dataItemCalls) {
					final DataDescriptionEntry calledDataDescriptionEntry = cobolDataDescriptionEntryService
							.getDataDescriptionEntry(call);

					if (calledDataDescriptionEntry != null) {
						if (cobolDataDescriptionEntryService.getDataDescriptionEntry(call)
								.equals(dataDescriptionEntry)) {
							dataItemInitialized = true;
							break;
						}
					}
				}
			}
		}

		return dataItemInitialized || dataItemMoved;
	}

	protected boolean parentInitialized(final DataDescriptionEntry dataDescriptionEntry) {
		final DataDescriptionEntryGroup parentDataDescriptionEntryGroup = dataDescriptionEntry
				.getParentDataDescriptionEntryGroup();
		final boolean result;

		if (parentDataDescriptionEntryGroup == null) {
			result = false;
		} else {
			result = isInitializedDataItem(parentDataDescriptionEntryGroup);
		}

		return result;
	}
}
