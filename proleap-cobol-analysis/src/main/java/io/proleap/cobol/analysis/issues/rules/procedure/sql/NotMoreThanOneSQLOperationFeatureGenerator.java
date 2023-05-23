package io.proleap.cobol.analysis.issues.rules.procedure.sql;

import java.util.stream.Stream;
import java.util.stream.Stream.Builder;

import javax.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class NotMoreThanOneSQLOperationFeatureGenerator extends FeatureGenerator<Statement> {

	@Override
	public Stream<Statement> getAll(final CompilationUnit compilationUnit) {
		final Stream<Paragraph> paragraphs = CobolStreamUtils.paragraphs(compilationUnit);
		final Stream<Section> sections = CobolStreamUtils.sections(compilationUnit);

		return Stream.concat(relevantFromParagraphs(paragraphs), relevantFromSections(sections));
	}

	protected boolean isRelevantStatement(final Builder<Statement> streamBuilder, boolean flag,
			final Statement statement) {
		if (StatementTypeEnum.EXEC_SQL.equals(statement.getStatementType())) {
			if (flag) {
				streamBuilder.add(statement);
			} else {
				flag = true;
			}
		}

		return flag;
	}

	protected Stream<Statement> relevantFromParagraphs(final Stream<Paragraph> paragraphs) {
		final Builder<Statement> streamBuilder = Stream.builder();

		paragraphs.forEach(paragraph -> {
			boolean flag = false;
			for (final Statement statement : paragraph.getStatements()) {
				flag = isRelevantStatement(streamBuilder, flag, statement);

			}
		});

		return streamBuilder.build();
	}

	protected Stream<Statement> relevantFromSections(final Stream<Section> sections) {
		final Builder<Statement> streamBuilder = Stream.builder();

		sections.forEach(section -> {
			boolean flag = false;
			for (final Statement statement : section.getStatements()) {
				flag = isRelevantStatement(streamBuilder, flag, statement);
			}
		});

		return streamBuilder.build();
	}
}
