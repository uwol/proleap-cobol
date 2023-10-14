package io.proleap.cobol.analysis.issues.rules.procedure.literal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolBaseVisitor;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Literal;
import io.proleap.cobol.asg.metamodel.Literal.LiteralType;
import io.proleap.cobol.asg.metamodel.Program;

@Singleton
public class StringLiteralsShouldNotBeDuplicatedFeatureGenerator extends FeatureGenerator<Literal> {

	@Override
	public Stream<Literal> getAll(final CompilationUnit compilationUnit) {
		final Program program = compilationUnit.getProgram();
		final List<Literal> result = new ArrayList<Literal>();
		final List<Object> literalList = new ArrayList<Object>();
		final Set<Literal> literalSet = new HashSet<Literal>();

		final CobolBaseVisitor<Boolean> visitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitLiteral(final CobolParser.LiteralContext ctx) {
				final Literal literal = (Literal) program.getASGElementRegistry().getASGElement(ctx);

				if (literal != null) {
					final LiteralType literalType = literal.getLiteralType();

					if (LiteralType.NON_NUMERIC.equals(literalType)) {
						literalList.add(literal.getValue());
						literalSet.add(literal);
					}
				}

				return true;
			}
		};

		visitor.visit(compilationUnit.getCtx());
		for (final Literal l : literalSet) {
			final int frequency = Collections.frequency(literalList, l.getValue());

			if (frequency > 1) {
				result.add(l);
			}
		}

		return result.stream();
	}
}
