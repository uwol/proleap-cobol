package io.proleap.cobol.transform.java.rules.lang.procedure.compute;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.ComputeStoreContext;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.procedure.compute.Store;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class StoreRule extends CobolTransformRule<ComputeStoreContext, Store> {

	@Override
	public void apply(final ComputeStoreContext ctx, final Store store, final RuleContext rc) {
		final Call storeCall = store.getStoreCall();
		rc.visit(storeCall.getCtx());
	}

	@Override
	public Class<ComputeStoreContext> from() {
		return ComputeStoreContext.class;
	}
}
