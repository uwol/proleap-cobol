package io.proleap.cobol.analysis.codexml;

import java.util.HashMap;
import java.util.Map;

import org.antlr.v4.runtime.tree.ParseTree;
import org.dom4j.Element;

import io.proleap.cobol.analysis.exception.CobolAnalysisException;
import io.proleap.cobol.analysis.util.ANTLRUtils;

public class CobolDomElementRegistry {

	protected final Map<ParseTree, Element> domElementRegistry = new HashMap<ParseTree, Element>();

	public Element findParentDomElement(final ParseTree ctx) {
		return ANTLRUtils.findParent(Element.class, ctx, domElementRegistry);
	}

	public void put(final ParseTree ctx, final Element element) {
		if (domElementRegistry.containsKey(ctx)) {
			throw new CobolAnalysisException("key " + ctx + " already exists in dom element registry");
		}

		domElementRegistry.put(ctx, element);
	}
}
