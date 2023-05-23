package io.proleap.cobol.analysis.issues.rules.procedure.move;

import java.math.BigDecimal;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.PictureClause;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveStatement;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveToSendingArea;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveToStatement;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.datadescription.CobolPictureLengthService;
import io.proleap.cobol.commons.type.CobolTypeEnum;
import io.proleap.cobol.commons.type.CobolTypeService;
import io.proleap.cobol.commons.util.CobolStreamUtils;
import io.proleap.cobol.commons.value.CobolValueService;
import io.proleap.cobol.commons.value.CobolValueStmtService;

@Singleton
public class MoveOnlyToLargeEnoughVariablesFeatureGenerator extends FeatureGenerator<Statement> {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Inject
	private CobolPictureLengthService cobolPictureLengthService;

	@Inject
	private CobolTypeService cobolTypeService;

	@Inject
	private CobolValueService valueService;

	@Inject
	private CobolValueStmtService valueStmtService;

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.statementsRec(compilationUnit).filter(statement -> {
			return isRelevantStatement(statement);
		});
	}

	protected boolean isDataDescriptionEntryLargeEnough(final ValueStmt sendingArea, final Call call) {
		final DataDescriptionEntry dataDescriptionEntry = cobolDataDescriptionEntryService
				.getDataDescriptionEntry(call);
		final boolean result;

		if (dataDescriptionEntry == null) {
			result = true;
		} else {
			final DataDescriptionEntryType dataDescriptionEntryType = dataDescriptionEntry
					.getDataDescriptionEntryType();

			switch (dataDescriptionEntryType) {
			case GROUP:
			case SCALAR:
				final DataDescriptionEntryGroup dataEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
				final CobolTypeEnum sendingAreaType = cobolTypeService.getType(sendingArea);

				if (CobolTypeEnum.INTEGER.equals(sendingAreaType)) {
					result = isIntegerLargeEnough(sendingArea, dataEntryGroup);
				} else if (CobolTypeEnum.STRING.equals(sendingAreaType)) {
					result = isStringLargeEnough(sendingArea, dataEntryGroup);
				} else if (CobolTypeEnum.FLOAT.equals(sendingAreaType)) {
					result = isFloatLargeEnough(sendingArea, dataEntryGroup);
				} else {
					result = true;
				}
				break;
			default:
				result = true;
				break;
			}
		}

		return result;
	}

	protected boolean isFloatLargeEnough(final ValueStmt sendingArea,
			final DataDescriptionEntryGroup dataDescriptionEntryGroup) {
		final BigDecimal sendingAreaValue = valueService.getDecimal(valueStmtService.getValue(sendingArea, null));
		final boolean result;

		if (sendingAreaValue == null) {
			result = true;
		} else {
			final PictureClause pictureClause = dataDescriptionEntryGroup.getPictureClause();

			if (pictureClause == null) {
				result = true;
			} else {
				final String pictureString = pictureClause.getPictureString();

				if (pictureString == null || pictureString.isBlank()) {
					result = true;
				} else {
					final String[] decimals = String.valueOf(sendingAreaValue).split("[.]");
					final boolean decimalIsLargeEnough;
					final boolean fractionIsLargeEnough;

					if (decimals.length > 0) {
						final String decimal = decimals[0];
						final int decimalLength = decimal.length();

						final Integer pictureDecimalLength = cobolPictureLengthService
								.getIntegerPartLength(pictureString);

						if (pictureDecimalLength == null) {
							decimalIsLargeEnough = true;
						} else {
							decimalIsLargeEnough = pictureDecimalLength >= decimalLength;
						}
					} else {
						decimalIsLargeEnough = true;
					}

					if (decimals.length > 1) {
						final String fraction = decimals[1];
						final int fractionLength = fraction.length();

						final Integer pictureFractionLength = cobolPictureLengthService
								.getFractionalPartLength(pictureString);

						if (pictureFractionLength == null) {
							fractionIsLargeEnough = true;
						} else {
							fractionIsLargeEnough = pictureFractionLength >= fractionLength;
						}
					} else {
						fractionIsLargeEnough = true;
					}

					result = decimalIsLargeEnough && fractionIsLargeEnough;
				}
			}
		}

		return result;
	}

	protected boolean isIntegerLargeEnough(final ValueStmt sendingArea,
			final DataDescriptionEntryGroup dataDescriptionEntryGroup) {
		final BigDecimal sendingAreaValue = valueService.getDecimal(valueStmtService.getValue(sendingArea, null));
		final boolean result;

		if (sendingAreaValue == null) {
			result = true;
		} else {
			final String valueOfSendingArea = String.valueOf(sendingAreaValue.abs());
			final int sendingAreaLength = valueOfSendingArea.length();
			final PictureClause pictureClause = dataDescriptionEntryGroup.getPictureClause();

			if (pictureClause == null) {
				result = true;
			} else {
				final String pictureString = pictureClause.getPictureString();

				if (pictureString == null || pictureString.isBlank()) {
					result = true;
				} else {
					final Integer pictureLength = cobolPictureLengthService.getIntegerPartLength(pictureString);

					if (pictureLength == null) {
						result = true;
					} else {
						result = pictureLength >= sendingAreaLength;
					}
				}
			}
		}

		return result;
	}

	protected boolean isRelevantStatement(final Statement statement) {
		final boolean isMoveStatement = StatementTypeEnum.MOVE.equals(statement.getStatementType());
		boolean result = false;

		if (!isMoveStatement) {
			result = false;
		} else {
			final MoveStatement moveStatement = (MoveStatement) statement;
			final boolean isMoveToStatement = MoveStatement.MoveType.MOVE_TO.equals(moveStatement.getMoveType());

			if (!isMoveToStatement) {
				result = false;
			} else {
				final MoveToStatement moveToStatement = moveStatement.getMoveToStatement();
				final MoveToSendingArea moveToSendingArea = moveToStatement.getSendingArea();
				final ValueStmt sendingArea = moveToSendingArea.getSendingAreaValueStmt();

				for (final Call call : moveToStatement.getReceivingAreaCalls()) {
					final boolean dataDescriptionEntryLargeEnough = isDataDescriptionEntryLargeEnough(sendingArea,
							call);

					if (!dataDescriptionEntryLargeEnough) {
						result = true;
						break;
					}
				}
			}
		}

		return result;
	}

	protected boolean isStringLargeEnough(final ValueStmt sendingArea,
			final DataDescriptionEntryGroup dataDescriptionEntryGroup) {
		final String sendingAreaValue = valueService.getString(valueStmtService.getValue(sendingArea, null));
		final boolean result;

		if (sendingAreaValue == null) {
			result = true;
		} else {
			final int sendingAreaLength = sendingAreaValue.length();
			final PictureClause pictureClause = dataDescriptionEntryGroup.getPictureClause();

			if (pictureClause == null) {
				result = true;
			} else {
				final String pictureString = pictureClause.getPictureString();

				if (pictureString == null || pictureString.isBlank()) {
					result = true;
				} else {
					final Integer pictureLength = cobolPictureLengthService.getStringLength(pictureString);

					if (pictureLength == null) {
						result = true;
					} else {
						result = pictureLength >= sendingAreaLength;
					}
				}
			}
		}

		return result;
	}
}
