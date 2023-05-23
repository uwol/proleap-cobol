package io.proleap.cobol.analysis.issues.rules.procedure.condition;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.CobolBaseVisitor;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry.DataDescriptionEntryType;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.PictureClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.UsageClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.UsageClause.UsageClauseType;
import io.proleap.cobol.asg.metamodel.valuestmt.ArithmeticValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.RelationConditionValueStmt;
import io.proleap.cobol.asg.metamodel.valuestmt.relation.ArithmeticComparison;
import io.proleap.cobol.commons.datadescription.CobolDataDescriptionEntryService;
import io.proleap.cobol.commons.datadescription.CobolPictureLengthService;

@Singleton
public class VariablesOfDifferentNumericFormatFeatureGenerator extends FeatureGenerator<RelationConditionValueStmt> {

	@Inject
	private CobolDataDescriptionEntryService cobolDataDescriptionEntryService;

	@Inject
	private CobolPictureLengthService cobolPictureLengthService;

	protected boolean differentDecimalPlaces(final PictureClause pictureClauseLeft,
			final PictureClause pictureClauseRight) {
		final String pictureStringLeft = pictureClauseLeft.getPictureString();
		final String pictureStringRight = pictureClauseRight.getPictureString();

		final Integer decimalLengthLeft = cobolPictureLengthService.getFractionalPartLength(pictureStringLeft);
		final Integer decimalLengthRight = cobolPictureLengthService.getFractionalPartLength(pictureStringRight);

		final boolean result;

		if (decimalLengthLeft == null) {
			result = false;
		} else if (decimalLengthRight == null) {
			result = false;
		} else {
			if (decimalLengthLeft.equals(decimalLengthRight)) {
				result = false;
			} else {
				result = true;
			}
		}

		return result;
	}

	protected boolean differentNumericFormat(final DataDescriptionEntry dataDescriptionEntryLeft,
			final DataDescriptionEntry dataDescriptionEntryRight) {
		final DataDescriptionEntryType dataDescriptionEntryTypeLeft = dataDescriptionEntryLeft
				.getDataDescriptionEntryType();
		final DataDescriptionEntryType dataDescriptionEntryTypeRight = dataDescriptionEntryRight
				.getDataDescriptionEntryType();
		final boolean result;

		if (!DataDescriptionEntry.DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryTypeLeft)) {
			result = false;
		} else if (!DataDescriptionEntry.DataDescriptionEntryType.GROUP.equals(dataDescriptionEntryTypeRight)) {
			result = false;
		} else {
			final DataDescriptionEntryGroup dataDescriptionEntryGroupLeft = (DataDescriptionEntryGroup) dataDescriptionEntryLeft;
			final DataDescriptionEntryGroup dataDescriptionEntryGroupRight = (DataDescriptionEntryGroup) dataDescriptionEntryRight;

			final UsageClause usageClauseLeft = dataDescriptionEntryGroupLeft.getUsageClause();
			final UsageClause usageClauseRight = dataDescriptionEntryGroupRight.getUsageClause();
			final boolean usageClauseResult;

			if (usageClauseLeft == null || usageClauseRight == null) {
				usageClauseResult = false;
			} else {
				usageClauseResult = differentUsageClause(usageClauseLeft, usageClauseRight);
			}

			final PictureClause pictureClauseLeft = dataDescriptionEntryGroupLeft.getPictureClause();
			final PictureClause pictureClauseRight = dataDescriptionEntryGroupRight.getPictureClause();
			final boolean pictureClauseResult;

			if (pictureClauseLeft == null || pictureClauseRight == null) {
				pictureClauseResult = false;
			} else {
				pictureClauseResult = differentDecimalPlaces(pictureClauseLeft, pictureClauseRight);
			}

			result = usageClauseResult || pictureClauseResult;
		}

		return result;
	}

	protected boolean differentUsageClause(final UsageClause usageClauseLeft, final UsageClause usageClauseRight) {
		final boolean result;

		if (usageClauseLeft == null) {
			result = false;
		} else if (usageClauseRight == null) {
			result = false;
		} else {
			final UsageClauseType usageClauseTypeLeft = usageClauseLeft.getUsageClauseType();
			final UsageClauseType usageClauseTypeRight = usageClauseRight.getUsageClauseType();

			if (usageClauseTypeLeft.equals(usageClauseTypeRight)) {
				result = false;
			} else {
				result = true;
			}
		}

		return result;
	}

	@Override
	public Stream<RelationConditionValueStmt> getAll(final CompilationUnit compilationUnit) {
		final Program program = compilationUnit.getProgram();
		final List<RelationConditionValueStmt> result = new ArrayList<RelationConditionValueStmt>();

		final CobolBaseVisitor<Boolean> visitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitRelationCondition(final CobolParser.RelationConditionContext ctx) {
				final RelationConditionValueStmt condition = (RelationConditionValueStmt) program
						.getASGElementRegistry().getASGElement(ctx);

				if (isRelevantRelation(condition)) {
					result.add(condition);
				}

				return true;
			}
		};

		visitor.visit(compilationUnit.getCtx());
		return result.stream();
	}

	protected boolean isRelevantRelation(final RelationConditionValueStmt condition) {
		final ArithmeticComparison arithmeticComparison = condition.getArithmeticComparison();
		final boolean result;

		if (arithmeticComparison == null) {
			result = false;
		} else {
			final ArithmeticValueStmt arithmeticExpressionLeft = arithmeticComparison.getArithmeticExpressionLeft();
			final ArithmeticValueStmt arithmeticExpressionRight = arithmeticComparison.getArithmeticExpressionRight();

			if (arithmeticExpressionLeft == null) {
				result = false;
			} else if (arithmeticExpressionRight == null) {
				result = false;
			} else {
				final DataDescriptionEntry dataDescriptionEntryRight = cobolDataDescriptionEntryService
						.getDataDescriptionEntry(arithmeticExpressionRight);
				final DataDescriptionEntry dataDescriptionEntryLeft = cobolDataDescriptionEntryService
						.getDataDescriptionEntry(arithmeticExpressionLeft);

				if (dataDescriptionEntryLeft == null) {
					result = false;
				} else if (dataDescriptionEntryRight == null) {
					result = false;
				} else {
					result = differentNumericFormat(dataDescriptionEntryLeft, dataDescriptionEntryRight);
				}
			}
		}

		return result;
	}
}
