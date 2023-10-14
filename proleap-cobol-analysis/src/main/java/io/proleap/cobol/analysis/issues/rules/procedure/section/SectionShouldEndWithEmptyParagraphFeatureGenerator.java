package io.proleap.cobol.analysis.issues.rules.procedure.section;

import java.util.List;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementType;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.stop.StopStatement;
import io.proleap.cobol.asg.metamodel.procedure.stop.StopStatement.StopType;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class SectionShouldEndWithEmptyParagraphFeatureGenerator extends FeatureGenerator<Section> {

	@Override
	public Stream<Section> getAll(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.sections(compilationUnit).filter(section -> isRelevantSection(section));
	}

	protected boolean isRelevantSection(final Section section) {
		final boolean result;
		final List<Paragraph> paragraphs = section.getParagraphs();

		if (paragraphs.isEmpty()) {
			result = false;
		} else {
			final Paragraph lastParagraph = paragraphs.get(paragraphs.size() - 1);
			final List<Statement> statements = lastParagraph.getStatements();

			if (statements.isEmpty()) {
				result = false;
			} else if (statements.size() == 1) {
				final Statement statement = statements.get(0);
				final StatementType statementType = statement.getStatementType();

				if (StatementTypeEnum.EXIT.equals(statementType)) {
					result = false;
				} else if (StatementTypeEnum.STOP.equals(statementType)) {
					final StopStatement stopStatement = (StopStatement) statement;
					final StopType stopType = stopStatement.getStopType();

					if (StopType.STOP_RUN.equals(stopType)) {
						result = false;
					} else {
						result = true;
					}
				} else if (StatementTypeEnum.GO_BACK.equals(statementType)) {
					result = false;
				} else {
					result = true;
				}
			} else {
				result = true;
			}
		}

		return result;
	}
}
