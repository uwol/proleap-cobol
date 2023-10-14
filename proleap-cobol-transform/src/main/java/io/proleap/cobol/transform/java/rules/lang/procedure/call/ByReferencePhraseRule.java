package io.proleap.cobol.transform.java.rules.lang.procedure.call;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.CallByReferencePhraseContext;
import io.proleap.cobol.asg.metamodel.procedure.call.ByReference;
import io.proleap.cobol.asg.metamodel.procedure.call.ByReferencePhrase;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class ByReferencePhraseRule extends CobolTransformRule<CallByReferencePhraseContext, ByReferencePhrase> {

	@Override
	public void apply(final CallByReferencePhraseContext ctx, final ByReferencePhrase byReferencePhrase,
			final RuleContext rc) {
		for (final ByReference byReference : byReferencePhrase.getByReferences()) {
			rc.visit(byReference.getCtx());
		}
	}

	@Override
	public Class<CallByReferencePhraseContext> from() {
		return CallByReferencePhraseContext.class;
	}
}
