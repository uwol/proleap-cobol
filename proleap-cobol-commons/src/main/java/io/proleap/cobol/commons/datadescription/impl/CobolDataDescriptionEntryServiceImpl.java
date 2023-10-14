package io.proleap.cobol.commons.datadescription.impl;

import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.DataDescriptionEntryCall;
import io.proleap.cobol.asg.metamodel.call.TableCall;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryRename;
import io.proleap.cobol.asg.metamodel.data.datadescription.RenamesClause;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.CallValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Basis;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDiv;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.MultDivs;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.PlusMinus;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Power;
import io.proleap.cobol.asg.metamodel.valuestmt.arithmetic.Powers;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;

@Singleton
public class CobolDataDescriptionEntryServiceImpl implements CobolDataDescriptionEntryService {

	@Override
	public DataDescriptionEntry findEffectiveDataDescriptionEntry(final DataDescriptionEntry dataDescriptionEntry) {
		final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry.getDataDescriptionEntryType();
		final DataDescriptionEntry result;

		if (!DataDescriptionEntryType.RENAME.equals(dataDescriptionEntryType)) {
			result = dataDescriptionEntry;
		} else {
			final DataDescriptionEntryRename dataDescriptionEntryRename = (DataDescriptionEntryRename) dataDescriptionEntry;
			final RenamesClause renamesClause = dataDescriptionEntryRename.getRenamesClause();
			result = getDataDescriptionEntry(renamesClause.getFrom());
		}

		return result;
	}

	@Override
	public DataDescriptionEntry getDataDescriptionEntry(final Call call) {
		final DataDescriptionEntry dataDescriptionEntry;

		if (CallType.DATA_DESCRIPTION_ENTRY_CALL.equals(call.getCallType())) {
			final DataDescriptionEntryCall dataDescriptionEntryCall = (DataDescriptionEntryCall) call.unwrap();
			dataDescriptionEntry = dataDescriptionEntryCall.getDataDescriptionEntry();
		} else if (CallType.TABLE_CALL.equals(call.getCallType())) {
			final TableCall tableCall = (TableCall) call.unwrap();
			dataDescriptionEntry = tableCall.getDataDescriptionEntry();
		} else {
			dataDescriptionEntry = null;
		}

		final DataDescriptionEntry result;

		if (dataDescriptionEntry == null) {
			result = null;
		} else {
			result = findEffectiveDataDescriptionEntry(dataDescriptionEntry);
		}

		return result;
	}

	@Override
	public DataDescriptionEntry getDataDescriptionEntry(final ValueStmt valueStmt) {
		final DataDescriptionEntry result;

		if (valueStmt instanceof ArithmeticValueStmt) {
			final ArithmeticValueStmt arithmeticValueStmt = (ArithmeticValueStmt) valueStmt;

			if (!arithmeticValueStmt.getPlusMinus().isEmpty()) {
				result = null;
			} else {
				result = getDataDescriptionEntry(arithmeticValueStmt.getMultDivs());
			}
		} else if (valueStmt instanceof Basis) {
			final Basis basis = (Basis) valueStmt;
			result = getDataDescriptionEntry(basis.getBasisValueStmt());
		} else if (valueStmt instanceof CallValueStmt) {
			final CallValueStmt callValueStmt = (CallValueStmt) valueStmt;
			final Call call = callValueStmt.getCall();
			result = getDataDescriptionEntry(call);
		} else if (valueStmt instanceof MultDiv) {
			final MultDiv multDiv = (MultDiv) valueStmt;
			result = getDataDescriptionEntry(multDiv.getPowers());
		} else if (valueStmt instanceof MultDivs) {
			final MultDivs multDivs = (MultDivs) valueStmt;

			if (!multDivs.getMultDivs().isEmpty()) {
				result = null;
			} else {
				result = getDataDescriptionEntry(multDivs.getPowers());
			}
		} else if (valueStmt instanceof PlusMinus) {
			final PlusMinus plusMinus = (PlusMinus) valueStmt;
			result = getDataDescriptionEntry(plusMinus.getMultDivs());
		} else if (valueStmt instanceof Power) {
			final Power power = (Power) valueStmt;
			result = getDataDescriptionEntry(power.getBasis());
		} else if (valueStmt instanceof Powers) {
			final Powers powers = (Powers) valueStmt;

			if (!powers.getPowers().isEmpty()) {
				result = null;
			} else {
				result = getDataDescriptionEntry(powers.getBasis());
			}
		} else {
			result = null;
		}

		return result;
	}

	@Override
	public boolean hasChildren(final DataDescriptionEntry dataDescriptionEntry) {
		final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry.getDataDescriptionEntryType();
		final boolean result;

		switch (dataDescriptionEntryType) {
		case GROUP:
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;

			result = dataDescriptionEntryGroup.getDataDescriptionEntries().stream().map(entry -> {
				return entry.getDataDescriptionEntryType();
			}).filter(type -> {
				return !DataDescriptionEntryType.CONDITION.equals(type);
			}).count() > 0;
			break;
		default:
			result = false;
			break;
		}

		return result;
	}
}
