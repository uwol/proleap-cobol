package io.proleap.cobol.analysis.issues.rules.procedure.literal;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolBaseVisitor;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.IntegerLiteral;
import io.proleap.cobol.asg.metamodel.Literal;
import io.proleap.cobol.asg.metamodel.Literal.LiteralType;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.registry.ASGElementRegistry;

@Singleton
public class MagicNumberShouldNotBeUsedFeatureGenerator extends FeatureGenerator<ASGElement> {

	@Override
	public Stream<ASGElement> getAll(final CompilationUnit compilationUnit) {
		final Program program = compilationUnit.getProgram();
		final List<ASGElement> result = new ArrayList<ASGElement>();

		final CobolBaseVisitor<Boolean> integerLiteralVisitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitIntegerLiteral(final CobolParser.IntegerLiteralContext ctx) {
				final ASGElementRegistry asgElementRegistry = program.getASGElementRegistry();
				final IntegerLiteral integerLiteral = (IntegerLiteral) asgElementRegistry.getASGElement(ctx);

				if (integerLiteral != null) {
					result.add(integerLiteral);
				}

				return true;
			}
		};

		final CobolBaseVisitor<Boolean> literalVisitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitLiteral(final CobolParser.LiteralContext ctx) {
				final ASGElementRegistry asgElementRegistry = program.getASGElementRegistry();
				final Literal literal = (Literal) asgElementRegistry.getASGElement(ctx);

				if (literal != null) {
					final LiteralType literalType = literal.getLiteralType();

					if (LiteralType.NUMERIC.equals(literalType)) {
						result.add(literal);
					}
				}

				return true;
			}
		};

		compilationUnit.getProgramUnits().stream().map(programUnit -> programUnit.getProcedureDivision())
				.filter(procedureDivision -> {
					return procedureDivision != null;
				}).forEach(procedureDivision -> {
					literalVisitor.visit(procedureDivision.getCtx());
					integerLiteralVisitor.visit(procedureDivision.getCtx());
				});

		return result.stream();
	}
}
