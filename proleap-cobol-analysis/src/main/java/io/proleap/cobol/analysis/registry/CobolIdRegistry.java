package io.proleap.cobol.analysis.registry;

import java.util.HashMap;
import java.util.Map;

import org.antlr.v4.runtime.ParserRuleContext;

import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.registry.ASGElementRegistry;
import io.proleap.cobol.asg.util.ANTLRUtils;

public class CobolIdRegistry {

	private static final String SHARP = "#";

	protected final Map<ParserRuleContext, Long> ctxToIdRegistry = new HashMap<ParserRuleContext, Long>();

	protected Long currentId = 0l;

	protected final Map<Long, ParserRuleContext> idToCtxRegistry = new HashMap<Long, ParserRuleContext>();

	public String assureAbsoluteId(final ParserRuleContext ctx, final ASGElementRegistry asgElementRegistry) {
		final CompilationUnit parentCompilationUnit = (CompilationUnit) ANTLRUtils.findParent(CompilationUnit.class,
				ctx, asgElementRegistry);
		final String id = assureId(ctx);
		return parentCompilationUnit.getName() + SHARP + id;
	}

	public String assureId(final ParserRuleContext ctx) {
		Long id = ctxToIdRegistry.get(ctx);

		if (id == null) {
			id = currentId++;

			ctxToIdRegistry.put(ctx, id);
			idToCtxRegistry.put(id, ctx);
		}

		return String.valueOf(id);
	}

	public String assureRelativeId(final ParserRuleContext ctx, final CompilationUnit compilationUnit) {
		final ASGElementRegistry asgElementRegistry = compilationUnit.getProgram().getASGElementRegistry();
		final CompilationUnit parentCompilationUnit = (CompilationUnit) ANTLRUtils.findParent(CompilationUnit.class,
				ctx, asgElementRegistry);
		final String id = assureId(ctx);
		final String result;

		if (compilationUnit == parentCompilationUnit) {
			result = SHARP + id;
		} else if (parentCompilationUnit != null) {
			result = parentCompilationUnit.getName() + SHARP + id;
		} else {
			result = null;
		}

		return result;
	}

	public ParserRuleContext findForId(final Long id) {
		return idToCtxRegistry.get(id);
	}
}
