package io.proleap.cobol.transform.java.rules.lang.data;

import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.DataDivisionContext;
import io.proleap.cobol.asg.metamodel.data.DataDivision;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class DataDivisionRule extends CobolTransformRule<DataDivisionContext, DataDivision> {

	@Override
	public void apply(final DataDivisionContext ctx, final DataDivision dataDivision, final RuleContext rc) {
		rc.p("@Inject");
		rc.pNl(dataDivision);
		rc.p("EntityService entityService;");
		rc.pNl();
		rc.pNl();

		rc.visitChildren(ctx);
	}

	@Override
	public Class<DataDivisionContext> from() {
		return DataDivisionContext.class;
	}
}
