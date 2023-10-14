package io.proleap.cobol.commons.value.impl;

import java.math.BigDecimal;

import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.environment.EnvironmentDivision;
import io.proleap.cobol.asg.metamodel.environment.specialnames.DecimalPointClause;
import io.proleap.cobol.asg.metamodel.environment.specialnames.SpecialNamesParagraph;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.domain.CobolBooleanValue;
import io.proleap.cobol.commons.value.domain.CobolDecimalValue;
import io.proleap.cobol.commons.value.domain.CobolStringValue;
import io.proleap.cobol.commons.value.domain.CobolValue;
import io.proleap.cobol.commons.value.domain.CobolValue.CobolValueType;

@Singleton
public class CobolValueServiceImpl implements CobolValueService {

	private String formatBigDecimal(final BigDecimal decimalValue, final ProgramUnit programUnit) {
		final EnvironmentDivision environmentDivision = programUnit == null ? null
				: programUnit.getEnvironmentDivision();
		final SpecialNamesParagraph specialNamesParagraph = environmentDivision == null ? null
				: environmentDivision.getSpecialNamesParagraph();
		final DecimalPointClause decimalPointClause = specialNamesParagraph == null ? null
				: specialNamesParagraph.getDecimalPointClause();
		return decimalPointClause == null ? decimalValue.toPlainString()
				: decimalValue.toPlainString().replace(".", ",");
	}

	@Override
	public BigDecimal getAsDecimal(final CobolValue cobolValue) {
		final BigDecimal decimal = getDecimal(cobolValue);
		final BigDecimal result;

		if (decimal != null) {
			result = decimal;
		} else {
			final String stringValue = getString(cobolValue);

			if (stringValue != null) {
				final String trimmedStringValue = stringValue.trim();
				result = new BigDecimal(trimmedStringValue);
			} else {
				result = BigDecimal.ZERO;
			}
		}

		return result;
	}

	@Override
	public String getAsString(final CobolValue cobolValue, final ProgramUnit programUnit) {
		final String stringValue = getString(cobolValue);
		final String result;

		if (stringValue != null) {
			result = stringValue;
		} else {
			final BigDecimal decimalValue = getDecimal(cobolValue);

			if (decimalValue != null) {
				result = formatBigDecimal(decimalValue, programUnit);
			} else {
				final Boolean booleanValue = getBoolean(cobolValue);

				if (booleanValue != null) {
					result = booleanValue ? "1" : "0";
				} else {
					result = "";
				}
			}
		}

		return result;
	}

	@Override
	public Boolean getBoolean(final CobolValue cobolValue) {
		return cobolValue != null && CobolValueType.BOOLEAN.equals(cobolValue.getType())
				? ((CobolBooleanValue) cobolValue).getBoolean()
				: null;
	}

	@Override
	public BigDecimal getDecimal(final CobolValue cobolValue) {
		return cobolValue != null && CobolValueType.DECIMAL.equals(cobolValue.getType())
				? ((CobolDecimalValue) cobolValue).getDecimal()
				: null;
	}

	@Override
	public String getString(final CobolValue cobolValue) {
		return cobolValue != null && CobolValueType.STRING.equals(cobolValue.getType())
				? ((CobolStringValue) cobolValue).getString()
				: null;
	}

	@Override
	public Object getValue(final CobolValue cobolValue) {
		final Object result;

		switch (cobolValue.getType()) {
		case BOOLEAN:
			result = getBoolean(cobolValue);
			break;
		case DECIMAL:
			result = getDecimal(cobolValue);
			break;
		case STRING:
			result = getString(cobolValue);
			break;
		case HIGH_VALUE:
		case LOW_VALUE:
		default:
			result = null;
			break;
		}

		return result;
	}
}
