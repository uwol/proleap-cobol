package io.proleap.cobol.analysis.util;

import java.util.ArrayList;
import java.util.List;

import org.antlr.v4.runtime.tree.ParseTree;

import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.NamedElement;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.registry.ASGElementRegistry;
import io.proleap.cobol.asg.util.ANTLRUtils;

public class NamingUtils {

	public static String determineFullQualifiedName(final ParseTree ctx, final CompilationUnit compilationUnit) {
		final Program program = compilationUnit.getProgram();
		final ASGElementRegistry asgElementRegistry = program.getASGElementRegistry();

		final List<String> names = new ArrayList<>();
		names.add(compilationUnit.getName());

		final Paragraph paragraph = find(Paragraph.class, ctx, asgElementRegistry);
		final DataDescriptionEntry dataDescriptionEntry = find(DataDescriptionEntryGroup.class, ctx,
				asgElementRegistry);

		if (paragraph != null) {
			names.add(getNameString(paragraph));
		}

		if (dataDescriptionEntry != null) {
			names.add(getNameString(dataDescriptionEntry));
		}

		return String.join(".", names);
	}

	@SuppressWarnings("unchecked")
	protected static <T extends ASGElement> T find(final Class<? extends ASGElement> type, final ParseTree ctx,
			final ASGElementRegistry asgElementRegistry) {
		final ASGElement asgElement = asgElementRegistry.getASGElement(ctx);
		final boolean isInstance = type.isAssignableFrom(asgElement.getClass());
		final T result = isInstance ? (T) asgElement : ANTLRUtils.findParent(type, ctx, asgElementRegistry);
		return result;
	}

	protected static String getNameString(final NamedElement namedElement) {
		return namedElement != null ? namedElement.getName() : "";
	}
}
