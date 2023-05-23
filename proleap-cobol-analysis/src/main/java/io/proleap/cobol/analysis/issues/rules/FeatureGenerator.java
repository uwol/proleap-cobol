package io.proleap.cobol.analysis.issues.rules;

import java.util.stream.Stream;

import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.CompilationUnit;

public abstract class FeatureGenerator<T extends ASGElement> {

	public abstract Stream<T> getAll(CompilationUnit compilationUnit);

	@Override
	public String toString() {
		return getClass().getSimpleName();
	}
}
