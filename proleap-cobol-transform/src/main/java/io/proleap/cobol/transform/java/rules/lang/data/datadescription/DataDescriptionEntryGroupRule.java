package io.proleap.cobol.transform.java.rules.lang.data.datadescription;

import java.util.List;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.proleap.cobol.CobolParser.DataDescriptionEntryFormat1Context;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.PictureClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.ValueClause;
import io.proleap.cobol.asg.metamodel.data.datadescription.ValueInterval;
import io.proleap.cobol.commons.datadescription.CobolPictureLengthService;
import io.proleap.cobol.commons.datadescription.CobolPictureStringService;
import io.proleap.cobol.commons.type.CobolTypeEnum;
import io.proleap.cobol.commons.type.CobolTypeService;
import io.proleap.cobol.transform.java.identifier.method.JavaGetterIdentifierService;
import io.proleap.cobol.transform.java.identifier.method.JavaSetterIdentifierService;
import io.proleap.cobol.transform.java.identifier.variable.JavaVariableIdentifierService;
import io.proleap.cobol.transform.java.type.JavaInstanceService;
import io.proleap.cobol.transform.java.type.JavaInterfaceService;
import io.proleap.cobol.transform.java.type.JavaTypeService;
import io.proleap.cobol.transform.rule.CobolTransformRule;
import io.proleap.cobol.transform.rule.RuleContext;

@Singleton
public class DataDescriptionEntryGroupRule
		extends CobolTransformRule<DataDescriptionEntryFormat1Context, DataDescriptionEntryGroup> {

	public static boolean GENERATE_ACCESSORS = false;

	@Inject
	private CobolPictureLengthService cobolPictureLengthService;

	@Inject
	private CobolTypeService cobolTypeService;

	@Inject
	private JavaGetterIdentifierService javaGetterIdentifierService;

	@Inject
	private JavaInstanceService javaInstanceService;

	@Inject
	private JavaInterfaceService javaInterfaceService;

	@Inject
	private JavaSetterIdentifierService javaSetterIdentifierService;

	@Inject
	private JavaTypeService javaTypeService;

	@Inject
	private JavaVariableIdentifierService javaVariableIdentifierService;

	@Inject
	private CobolPictureStringService pictureStringService;

	@Override
	public void apply(final DataDescriptionEntryFormat1Context ctx,
			final DataDescriptionEntryGroup dataDescriptionEntryGroup, final RuleContext rc) {
		final List<DataDescriptionEntry> cotainedDataDescriptionEntries = dataDescriptionEntryGroup
				.getDataDescriptionEntries();

		if (cotainedDataDescriptionEntries.isEmpty()) {
			printScalarVariable(dataDescriptionEntryGroup, rc);

			if (GENERATE_ACCESSORS) {
				printGetter(dataDescriptionEntryGroup, rc);
				printSetter(dataDescriptionEntryGroup, rc);
			}
		} else {
			printClass(dataDescriptionEntryGroup, rc);
			printInstanceVariable(dataDescriptionEntryGroup, rc);

			if (GENERATE_ACCESSORS) {
				printGetter(dataDescriptionEntryGroup, rc);

				final int numberOfOccurrs = pictureStringService.getMaxOccurs(dataDescriptionEntryGroup);

				if (numberOfOccurrs == 1) {
					printSetter(dataDescriptionEntryGroup, rc);
				}
			}
		}
	}

	@Override
	public Class<DataDescriptionEntryFormat1Context> from() {
		return DataDescriptionEntryFormat1Context.class;
	}

	protected void printAnnotations(final DataDescriptionEntryGroup dataDescriptionEntryGroup, final RuleContext rc) {
		final PictureClause pictureClause = dataDescriptionEntryGroup.getPictureClause();

		if (pictureClause == null) {
		} else {
			final CobolTypeEnum type = cobolTypeService.getType(dataDescriptionEntryGroup);
			final String pictureString = pictureClause.getPictureString();

			if (CobolTypeEnum.INTEGER.equals(type)) {
				final Integer decimalLength = cobolPictureLengthService.getIntegerPartLength(pictureString);

				if (decimalLength == null) {
				} else {
					rc.p("@Digits(integer=%s, fraction=0)", decimalLength);
					rc.pNl(dataDescriptionEntryGroup);
				}
			} else if (CobolTypeEnum.STRING.equals(type)) {
				final Integer stringLength = cobolPictureLengthService.getStringLength(pictureString);

				if (stringLength == null) {
				} else {
					rc.p("@Size(max=%s)", stringLength);
					rc.pNl(dataDescriptionEntryGroup);
				}
			} else if (CobolTypeEnum.FLOAT.equals(type)) {
				final Integer decimalLength = cobolPictureLengthService.getIntegerPartLength(pictureString);
				final Integer fractionLength = cobolPictureLengthService.getFractionalPartLength(pictureString);

				if (decimalLength == null || fractionLength == null) {
				} else {
					rc.p("@Digits(integer=%s, fraction=%s)", decimalLength, fractionLength);
					rc.pNl(dataDescriptionEntryGroup);
				}
			}
		}
	}

	protected void printClass(final DataDescriptionEntryGroup dataDescriptionEntryGroup, final RuleContext rc) {
		rc.p("public class %s {", javaTypeService.mapToType(dataDescriptionEntryGroup));
		rc.pNl(dataDescriptionEntryGroup);
		rc.getPrinter().indent();

		for (final DataDescriptionEntry dataDescriptionEntry : dataDescriptionEntryGroup.getDataDescriptionEntries()) {
			rc.visit(dataDescriptionEntry.getCtx());
		}

		rc.getPrinter().unindent();
		rc.p("}");
		rc.pNl();
		rc.pNl();
	}

	protected void printGetter(final DataDescriptionEntryGroup dataDescriptionEntryGroup, final RuleContext rc) {
		rc.p("public %s %s", javaInterfaceService.mapToInterface(dataDescriptionEntryGroup),
				javaGetterIdentifierService.mapToIdentifier(dataDescriptionEntryGroup));
		rc.p("(){ ");
		rc.p("return %s; ", javaVariableIdentifierService.mapToIdentifier(dataDescriptionEntryGroup));
		rc.p("}");
		rc.pNl(dataDescriptionEntryGroup);
	}

	protected void printInstanceVariable(final DataDescriptionEntryGroup dataDescriptionEntryGroup,
			final RuleContext rc) {
		final String type = javaTypeService.mapToType(dataDescriptionEntryGroup);

		final String interfaceType = javaInterfaceService.mapToInterface(dataDescriptionEntryGroup);
		final String instanceType = javaInstanceService.mapToInstance(dataDescriptionEntryGroup);

		final String identifier = javaVariableIdentifierService.mapToIdentifier(dataDescriptionEntryGroup);
		final int numberOfOccurrs = pictureStringService.getMaxOccurs(dataDescriptionEntryGroup);

		if (numberOfOccurrs > 1) {
			rc.p("protected %s %s = new %s();", interfaceType, identifier, instanceType);
			rc.pNl();
			rc.p("{");
			rc.pNl();
			rc.getPrinter().indent();

			for (int i = 0; i < numberOfOccurrs; i++) {
				rc.p("%s.add(new %s());", identifier, type);
				rc.pNl();
			}

			rc.getPrinter().unindent();
			rc.p("}");
			rc.pNl();
		} else {
			rc.p("protected %s %s = new %s();", instanceType, identifier, instanceType);
			rc.pNl();
		}
	}

	protected void printScalarVariable(final DataDescriptionEntryGroup dataDescriptionEntryGroup,
			final RuleContext rc) {
		printAnnotations(dataDescriptionEntryGroup, rc);

		rc.p("protected %s %s", javaTypeService.mapToType(dataDescriptionEntryGroup),
				javaVariableIdentifierService.mapToIdentifier(dataDescriptionEntryGroup));

		if (dataDescriptionEntryGroup.getValueClause() != null) {
			rc.p(" = ");
			printValue(dataDescriptionEntryGroup.getValueClause(), rc);
		}

		rc.p(";");
		rc.pNl(dataDescriptionEntryGroup);
	}

	protected void printSetter(final DataDescriptionEntryGroup dataDescriptionEntryGroup, final RuleContext rc) {
		final String variableIdentifier = javaVariableIdentifierService.mapToIdentifier(dataDescriptionEntryGroup);

		rc.p("public void %s(%s %s){ ", javaSetterIdentifierService.mapToIdentifier(dataDescriptionEntryGroup),
				javaInterfaceService.mapToInterface(dataDescriptionEntryGroup), variableIdentifier);
		rc.p("this.%s = %s; ", variableIdentifier, variableIdentifier);
		rc.p("}");
		rc.pNl(dataDescriptionEntryGroup);
	}

	protected void printValue(final ValueClause valueClause, final RuleContext rc) {
		final List<ValueInterval> valueIntervals = valueClause.getValueIntervals();

		for (final ValueInterval valueInterval : valueIntervals) {
			rc.visit(valueInterval.getCtx());
		}
	}
}
