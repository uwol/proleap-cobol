package io.proleap.cobol.analysis.issues.rules.procedure.dataItemAccess;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolBaseVisitor;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.CobolParser.QualifiedInDataContext;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.call.Call;

@Singleton
public class DataItemsShouldNotBeAccesedUsingMoreThanOneOfClauseFeatureGenerator extends FeatureGenerator<Call> {

	@Override
	public Stream<Call> getAll(final CompilationUnit compilationUnit) {
		final Program program = compilationUnit.getProgram();
		final ArrayList<Call> callList = new ArrayList<Call>();

		final CobolBaseVisitor<Boolean> visitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitQualifiedDataNameFormat1(final CobolParser.QualifiedDataNameFormat1Context ctx) {
				final List<QualifiedInDataContext> qualifiedInData = ctx.qualifiedInData();
				final int size = qualifiedInData.size();

				if (size > 1) {
					callList.add((Call) program.getASGElementRegistry().getASGElement(ctx));
				}

				return true;
			}
		};

		visitor.visit(compilationUnit.getCtx());
		return callList.stream();
	}
}
