package io.proleap.cobol.analysis.issues.rules.data;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolBaseVisitor;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.TableCall;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.UsageClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.UsageClause.UsageClauseType;
import io.proleap.cobol.asg.metamodel.valuestmt.CallValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.Subscript;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;

@Singleton
public class BinaryVariablesShouldBeUsedForTableSubscriptAccessFeatureGenerator extends FeatureGenerator<TableCall> {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Override
	public Stream<TableCall> getAll(final CompilationUnit compilationUnit) {
		final Program program = compilationUnit.getProgram();
		final List<TableCall> result = new ArrayList<>();

		final CobolBaseVisitor<Boolean> visitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitTableCall(final CobolParser.TableCallContext ctx) {
				final TableCall tableCall = (TableCall) program.getASGElementRegistry().getASGElement(ctx);
				if (tableCall != null) {
					if (isRelevantTableCall(tableCall)) {
						result.add(tableCall);
					}
				}
				return true;
			}
		};

		visitor.visit(compilationUnit.getCtx());

		if (result.isEmpty()) {
			return Stream.empty();
		} else {
			return result.stream();
		}
	}

	protected boolean isBinaryVariable(final Call dependingOnCall) {
		final DataDescriptionEntry dataDescriptionEntry = cobolDataDescriptionEntryService
				.getDataDescriptionEntry(dependingOnCall);
		final boolean result;

		if (dataDescriptionEntry == null) {
			result = false;
		} else {
			final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry
					.getDataDescriptionEntryType();

			if (DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryType)
					|| DataDescriptionEntryType.SCALAR.equals(dataDescriptionEntryType)) {
				final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
				final UsageClause usageClause = dataDescriptionEntryGroup.getUsageClause();

				if (usageClause == null) {
					result = false;
				} else {
					final UsageClauseType usageClauseType = usageClause.getUsageClauseType();

					switch (usageClauseType) {
					case BINARY:
						result = true;
						break;
					case BINARY_EXTENDED:
						result = true;
						break;
					case BINARY_TRUNCATED:
						result = true;
						break;
					case COMP:
						result = true;
						break;
					case COMP_4:
						result = true;
						break;
					case COMP_5:
						result = true;
						break;
					default:
						result = false;
						break;
					}
				}
			} else {
				result = false;
			}
		}

		return result;
	}

	protected boolean isRelevantTableCall(final TableCall tableCall) {
		final List<Subscript> subscripts = tableCall.getSubscripts();
		final List<Call> subscriptCalls = new ArrayList<>();
		boolean result = false;

		for (final Subscript subscript : subscripts) {
			final ValueStmt valueStmt = subscript.getSubscriptValueStmt();

			if (valueStmt instanceof CallValueStmt) {
				final CallValueStmt callValueStmt = (CallValueStmt) valueStmt;
				final Call call = callValueStmt.getCall();

				subscriptCalls.add(call);
			} else {
				result = true;
			}
		}

		subscriptCalls.removeIf(called -> isBinaryVariable(called));

		if (!result && subscriptCalls.isEmpty()) {
			result = false;
		} else {
			result = true;
		}

		return result;
	}
}
