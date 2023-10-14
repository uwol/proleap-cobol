package io.proleap.cobol.transform.java.rules.lang;

import java.util.Arrays;
import java.util.List;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.CompilationUnitContext;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.transform.java.identifier.JavaIdentifierService;
import io.proleap.cobol.transform.java.identifier.method.JavaMethodIdentifierService;
import io.proleap.cobol.transform.java.type.JavaTypeService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class CompilationUnitRule extends CobolTransformRule<CompilationUnitContext, CompilationUnit> {

	protected final List<String> imports = Arrays.asList("javax.validation.constraints.*", "io.proleap.cobol.api.*",
			"io.proleap.cobol.api.data.*", "io.proleap.cobol.api.environment.configuration.object.*",
			"io.proleap.cobol.api.environment.configuration.source.*",
			"io.proleap.cobol.api.environment.inputoutput.filecontrol.*",
			"io.proleap.cobol.api.ProLeapCobolApiSpringConfig", "org.springframework.context.ApplicationContext",
			"org.springframework.context.annotation.AnnotationConfigApplicationContext");

	@Inject
	private JavaIdentifierService javaIdentifierService;

	@Inject
	private JavaMethodIdentifierService javaMethodIdentifierService;

	@Inject
	private JavaTypeService javaTypeService;

	@Override
	public void apply(final CompilationUnitContext ctx, final CompilationUnit compilationUnit, final RuleContext rc) {
		if (rc.getPackageName() != null && !rc.getPackageName().isEmpty()) {
			rc.p("package %s;", rc.getPackageName());
			rc.pNl();
			rc.pNl();
		}

		rc.p("import java.lang.String;");
		rc.pNl();
		rc.p("import java.math.BigDecimal;");
		rc.pNl();
		rc.p("import java.util.Arrays;");
		rc.pNl();
		rc.p("import java.util.ArrayList;");
		rc.pNl();
		rc.p("import java.util.List;");
		rc.pNl();
		rc.p("import jakarta.inject.Inject;");
		rc.pNl();
		rc.p("import jakarta.inject.Singleton;");
		rc.pNl();
		rc.pNl();

		for (final String importEntry : imports) {
			rc.p("import %s;", importEntry);
			rc.pNl();
		}

		rc.pNl();

		rc.p("@Singleton");
		rc.pNl();

		rc.p("public class %s {", javaTypeService.mapToType(compilationUnit.getName()));
		rc.pNl();

		rc.getPrinter().indent();
		rc.visitChildren(ctx);

		printMainMethod(compilationUnit, rc);

		rc.getPrinter().unindent();
		rc.p("}");
		rc.pNl();
	}

	@Override
	public Class<CompilationUnitContext> from() {
		return CompilationUnitContext.class;
	}

	protected void printMainMethod(final CompilationUnit compilationUnit, final RuleContext rc) {
		final ProgramUnit programUnit = compilationUnit.getProgramUnit();

		if (programUnit != null) {
			final ProcedureDivision procedureDivision = programUnit.getProcedureDivision();

			if (procedureDivision != null) {
				rc.pNl();

				rc.p("public static void main(String[] args) throws Exception {");
				rc.pNl();
				rc.getPrinter().indent();

				final String className = javaTypeService.mapToType(compilationUnit.getName());
				final String instanceName = javaIdentifierService.mapToIdentifier(compilationUnit.getName());

				rc.p("final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, %s.class);",
						className);
				rc.pNl();

				rc.p("final %s %s = context.getBean(%s.class);", className, instanceName, className);
				rc.pNl();

				final List<Statement> statements = procedureDivision.getStatements();
				final List<Paragraph> paragraphs = procedureDivision.getRootParagraphs();
				final List<Section> sections = procedureDivision.getSections();

				if (!statements.isEmpty()) {
					rc.p("%s.procedureDivision();", instanceName);
				} else if (!sections.isEmpty()) {
					rc.p("%s.%s();", instanceName, javaMethodIdentifierService.mapToIdentifier(sections.get(0)));
				} else if (!paragraphs.isEmpty()) {
					rc.p("%s.%s();", instanceName, javaMethodIdentifierService.mapToIdentifier(paragraphs.get(0)));
				}

				rc.pNl();

				rc.getPrinter().unindent();
				rc.p("}");
				rc.pNl();
			}
		}
	}
}
