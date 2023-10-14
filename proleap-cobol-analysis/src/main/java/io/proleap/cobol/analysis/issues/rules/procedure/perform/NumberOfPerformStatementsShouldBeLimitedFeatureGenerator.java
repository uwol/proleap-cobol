package io.proleap.cobol.analysis.issues.rules.procedure.perform;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement.PerformStatementType;
import io.proleap.cobol.commons.util.CobolStreamUtils;

@Singleton
public class NumberOfPerformStatementsShouldBeLimitedFeatureGenerator extends FeatureGenerator<ASGElement> {

	private static final int MAX_PERFORM_COMPILATION_UNIT = 100;

	private static final int MAX_PERFORM_IN_PARAGRAPH = 5;

	private static final int MAX_PERFORM_IN_SECTION = 5;

	protected boolean containsTooManyPerformStatements(final List<Statement> statements, final int maxNumber) {
		int numberOfPerformStatements = 0;
		for (final Statement statement : statements) {
			if (statement.getStatementType().equals(StatementTypeEnum.PERFORM)) {
				final PerformStatement performStatement = (PerformStatement) statement;
				if (PerformStatementType.PROCEDURE.equals(performStatement.getPerformStatementType())) {
					numberOfPerformStatements++;
					if (numberOfPerformStatements > maxNumber) {
						return true;
					}
				}
			}
		}
		return false;
	}

	@Override
	public Stream<ASGElement> getAll(final CompilationUnit compilationUnit) {

		final List<ASGElement> elements = new ArrayList<ASGElement>();

		if (isRelevant(compilationUnit)) {
			elements.add(compilationUnit.getProgramUnit());
		}
		elements.addAll(getSections(compilationUnit));
		elements.addAll(getParagraphs(compilationUnit));

		return elements.stream();
	}

	protected List<Paragraph> getParagraphs(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.paragraphs(compilationUnit).filter(paragraph -> {
			return isRelevant(paragraph);
		}).collect(Collectors.toList());

	}

	protected List<Section> getSections(final CompilationUnit compilationUnit) {
		return CobolStreamUtils.sections(compilationUnit).filter(section -> {
			return isRelevant(section);
		}).collect(Collectors.toList());
	}

	protected boolean isRelevant(final CompilationUnit compilationUnit) {
		final List<Statement> statements = CobolStreamUtils.statementsRec(compilationUnit).collect(Collectors.toList());
		return containsTooManyPerformStatements(statements, MAX_PERFORM_COMPILATION_UNIT);
	}

	protected boolean isRelevant(final Paragraph paragraph) {
		final List<Statement> statements = paragraph.getStatements();
		return containsTooManyPerformStatements(statements, MAX_PERFORM_IN_PARAGRAPH);
	}

	protected boolean isRelevant(final Section section) {
		final List<Statement> statements = section.getStatements();
		return containsTooManyPerformStatements(statements, MAX_PERFORM_IN_SECTION);

	}

}
