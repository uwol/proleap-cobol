package io.proleap.cobol.commons.datadescription.impl;

import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.IntegerLiteral;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.OccursClause;
import io.proleap.cobol.asg.metamodel.valuestmt.IntegerLiteralValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.datadescription.CobolPictureStringService;

@Singleton
public class CobolPictureStringServiceImpl implements CobolPictureStringService {

	@Override
	public int getMaxOccurs(final DataDescriptionEntryGroup dataDescriptionEntryGroup) {
		int result = 1;

		for (final OccursClause occursClause : dataDescriptionEntryGroup.getOccursClauses()) {
			final ValueStmt fromValueStmt = occursClause.getFrom();

			if (fromValueStmt instanceof IntegerLiteralValueStmt) {
				final IntegerLiteralValueStmt fromIntegerLiteralValueStmt = (IntegerLiteralValueStmt) fromValueStmt;
				final IntegerLiteral from = fromIntegerLiteralValueStmt.getLiteral();
				final Integer fromValue = from == null ? 0 : from.getValue().intValue();

				final IntegerLiteral to = occursClause.getTo();
				final int toValue = to == null ? 0 : to.getValue().intValue();

				final int occursMax = Math.max(fromValue, toValue);
				result = Math.max(result, occursMax);
			}
		}

		return result;
	}
}
