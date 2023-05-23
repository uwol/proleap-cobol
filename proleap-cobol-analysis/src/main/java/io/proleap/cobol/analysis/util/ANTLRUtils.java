package io.proleap.cobol.analysis.util;

import java.util.Map;

import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;
import org.antlr.v4.runtime.tree.RuleNode;
import org.dom4j.Element;

public class ANTLRUtils {

	@SuppressWarnings("unchecked")
	public static <T extends Object> T findParent(final Class<? extends Element> type, final ParseTree from,
			final Map<ParseTree, Element> registry) {
		T result = null;

		ParseTree currentCtx = from;

		while (result == null && currentCtx != null) {
			currentCtx = currentCtx.getParent();

			final Element dto = registry.get(currentCtx);

			if (dto != null && type.isAssignableFrom(dto.getClass())) {
				result = (T) dto;
			}
		}

		return result;
	}

	public static boolean visitChildrenFrom(final RuleNode node, final RuleNode fromChild,
			final ParseTreeVisitor<Boolean> visitor) {
		final int n = node.getChildCount();
		boolean foundFromChild = false;

		for (int i = 0; i < n; i++) {
			final ParseTree c = node.getChild(i);

			if (c == fromChild) {
				foundFromChild = true;
			} else if (foundFromChild) {
				c.accept(visitor);
			}
		}

		return true;
	}

	public static boolean visitChildrenTo(final RuleNode node, final RuleNode toChild,
			final ParseTreeVisitor<Boolean> visitor) {
		final int n = node.getChildCount();
		boolean foundToChild = false;

		for (int i = 0; i < n; i++) {
			final ParseTree c = node.getChild(i);

			if (c == toChild) {
				foundToChild = true;
			} else if (!foundToChild) {
				c.accept(visitor);
			}
		}

		return true;
	}
}
