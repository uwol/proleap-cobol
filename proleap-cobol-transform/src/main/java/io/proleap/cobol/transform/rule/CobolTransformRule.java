package io.proleap.cobol.transform.rule;

import org.antlr.v4.runtime.ParserRuleContext;

import io.proleap.cobol.asg.metamodel.ASGElement;

public abstract class CobolTransformRule<S extends ParserRuleContext, T extends ASGElement>
		implements Comparable<CobolTransformRule<S, T>> {

	public abstract void apply(S ctx, T asgElement, RuleContext ruleContext);

	@Override
	public int compareTo(final CobolTransformRule<S, T> r) {
		return r.getPriority().compareTo(this.getPriority());
	}

	public abstract Class<S> from();

	public Integer getPriority() {
		return 0;
	}

	@Override
	public String toString() {
		return getClass().getSimpleName();
	}

	public boolean where(final S ctx, final T asgElement, final RuleContext rc) {
		return true;
	}
}
