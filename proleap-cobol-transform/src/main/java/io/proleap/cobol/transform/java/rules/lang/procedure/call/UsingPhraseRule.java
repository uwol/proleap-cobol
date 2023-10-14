package io.proleap.cobol.transform.java.rules.lang.procedure.call;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.CallUsingPhraseContext;
import io.proleap.cobol.asg.metamodel.procedure.call.UsingParameter;
import io.proleap.cobol.asg.metamodel.procedure.call.UsingPhrase;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class UsingPhraseRule extends CobolTransformRule<CallUsingPhraseContext, UsingPhrase> {

	@Override
	public void apply(final CallUsingPhraseContext ctx, final UsingPhrase usingPhrase, final RuleContext rc) {
		for (final UsingParameter parameter : usingPhrase.getUsingParameters()) {
			rc.visit(parameter.getCtx());
		}
	}

	@Override
	public Class<CallUsingPhraseContext> from() {
		return CallUsingPhraseContext.class;
	}
}
