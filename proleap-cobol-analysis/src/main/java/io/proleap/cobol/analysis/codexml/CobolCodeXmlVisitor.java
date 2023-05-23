package io.proleap.cobol.analysis.codexml;

import java.util.List;
import java.util.stream.Collectors;

import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.dom4j.Document;
import org.dom4j.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.proleap.cobol.CobolBaseVisitor;
import io.proleap.cobol.CobolParser.AddStatementContext;
import io.proleap.cobol.CobolParser.CdNameContext;
import io.proleap.cobol.CobolParser.CobolWordContext;
import io.proleap.cobol.CobolParser.CommentEntryContext;
import io.proleap.cobol.CobolParser.CompilationUnitContext;
import io.proleap.cobol.CobolParser.DataDescNameContext;
import io.proleap.cobol.CobolParser.DataDescriptionEntryFormat1Context;
import io.proleap.cobol.CobolParser.DataDescriptionEntryFormat2Context;
import io.proleap.cobol.CobolParser.DataDescriptionEntryFormat3Context;
import io.proleap.cobol.CobolParser.DataDivisionContext;
import io.proleap.cobol.CobolParser.DataNameContext;
import io.proleap.cobol.CobolParser.DisplayStatementContext;
import io.proleap.cobol.CobolParser.EnvironmentDivisionContext;
import io.proleap.cobol.CobolParser.FileControlEntryContext;
import io.proleap.cobol.CobolParser.FileNameContext;
import io.proleap.cobol.CobolParser.GobackStatementContext;
import io.proleap.cobol.CobolParser.IdentificationDivisionContext;
import io.proleap.cobol.CobolParser.IdentifierContext;
import io.proleap.cobol.CobolParser.LiteralContext;
import io.proleap.cobol.CobolParser.LocalNameContext;
import io.proleap.cobol.CobolParser.MnemonicNameContext;
import io.proleap.cobol.CobolParser.MoveStatementContext;
import io.proleap.cobol.CobolParser.ParagraphContext;
import io.proleap.cobol.CobolParser.PerformStatementContext;
import io.proleap.cobol.CobolParser.ProcedureDivisionContext;
import io.proleap.cobol.CobolParser.ProcedureNameContext;
import io.proleap.cobol.CobolParser.ProcedureSectionContext;
import io.proleap.cobol.CobolParser.ProgramIdParagraphContext;
import io.proleap.cobol.CobolParser.ProgramUnitContext;
import io.proleap.cobol.CobolParser.RecordNameContext;
import io.proleap.cobol.CobolParser.ReportNameContext;
import io.proleap.cobol.CobolParser.SectionNameContext;
import io.proleap.cobol.CobolParser.StopStatementContext;
import io.proleap.cobol.CobolParser.SubtractStatementContext;
import io.proleap.cobol.CobolParser.TableCallContext;
import io.proleap.cobol.CobolParser.WorkingStorageSectionContext;
import io.proleap.cobol.analysis.registry.CobolIdRegistry;
import io.proleap.cobol.analysis.util.NamingUtils;
import io.proleap.cobol.analysis.util.TokenUtils;
import io.proleap.cobol.asg.metamodel.ASGElement;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.call.Call.CallType;
import io.proleap.cobol.asg.metamodel.call.CommunicationDescriptionEntryCall;
import io.proleap.cobol.asg.metamodel.call.DataDescriptionEntryCall;
import io.proleap.cobol.asg.metamodel.call.FileControlEntryCall;
import io.proleap.cobol.asg.metamodel.call.ProcedureCall;
import io.proleap.cobol.asg.metamodel.call.ReportDescriptionEntryCall;
import io.proleap.cobol.asg.metamodel.call.ScreenDescriptionEntryCall;
import io.proleap.cobol.asg.metamodel.call.TableCall;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.registry.ASGElementRegistry;

public class CobolCodeXmlVisitor extends CobolBaseVisitor<Boolean> {

	private static final String A = "a";

	private static final String CLASS = "class";

	private static final String COMMA = ",";

	private static final String DATA_CALLERS = "data-callers";

	private static final String DATA_HREFS = "data-hrefs";

	private static final String DIV = "div";

	private static final String HREF = "href";

	private static final String ID = "id";

	private final static Logger LOG = LoggerFactory.getLogger(CobolCodeXmlVisitor.class);

	private static final String PRE = "pre";

	private static final String SPAN = "span";

	private static final String X_C = "x-c";

	private static final String X_I = "x-i";

	private static final String X_L = "x-l";

	protected final CompilationUnit compilationUnit;

	protected final Document document;

	protected CobolDomElementRegistry domElementRegistry = new CobolDomElementRegistry();

	protected CobolIdRegistry idRegistry;

	public CobolCodeXmlVisitor(final CompilationUnit compilationUnit, final Document document,
			final CobolIdRegistry idRegistry) {
		this.compilationUnit = compilationUnit;
		this.document = document;
		this.idRegistry = idRegistry;
	}

	protected Element addA(final ParserRuleContext ctx) {
		return findParentDomElement(ctx).addElement(A);
	}

	protected Boolean addCall(final ParserRuleContext ctx) {
		final ASGElement asgElement = getASGElement(ctx);

		if (asgElement != null && asgElement instanceof Call) {
			final Call call = (Call) asgElement;
			final Call unwrappedCall = call.unwrap();
			final String hrefValue = getCallId(unwrappedCall);

			if (hrefValue != null && !hrefValue.isEmpty()) {
				final Element element = addA(ctx).addAttribute(ID, idRegistry.assureId(ctx)).addAttribute(HREF,
						hrefValue);
				domElementRegistry.put(ctx, element);
			}
		}

		return visitChildren(ctx);
	}

	protected Element addDiv(final ParserRuleContext ctx) {
		return findParentDomElement(ctx).addElement(DIV);
	}

	protected Element addSpan(final ParserRuleContext ctx) {
		return findParentDomElement(ctx).addElement(SPAN);
	}

	protected void addStatementIdSpan(final ParserRuleContext ctx) {
		final Element element = addSpan(ctx).addAttribute(ID, idRegistry.assureId(ctx));
		domElementRegistry.put(ctx, element);
	}

	protected <T extends ASGElement> T findParentASGElement(final Class<? extends ASGElement> type,
			final ParserRuleContext from) {
		return io.proleap.cobol.asg.util.ANTLRUtils.findParent(type, from,
				compilationUnit.getProgram().getASGElementRegistry());
	}

	protected Element findParentDomElement(final ParseTree ctx) {
		return domElementRegistry.findParentDomElement(ctx);
	}

	protected ASGElement getASGElement(final ParserRuleContext ctx) {
		final Program program = compilationUnit.getProgram();
		final ASGElementRegistry asgElementRegistry = program.getASGElementRegistry();
		return asgElementRegistry.getASGElement(ctx);
	}

	protected ParserRuleContext getCalledParseTree(final Call call) {
		final CallType callType = call.getCallType();
		final ParserRuleContext result;

		switch (callType) {
		case COMMUNICATION_DESCRIPTION_ENTRY_CALL:
			result = ((CommunicationDescriptionEntryCall) call).getCommunicationDescriptionEntry().getCtx();
			break;
		case DATA_DESCRIPTION_ENTRY_CALL:
			result = ((DataDescriptionEntryCall) call).getDataDescriptionEntry().getCtx();
			break;
		case FILE_CONTROL_ENTRY_CALL:
			result = ((FileControlEntryCall) call).getFileControlEntry().getCtx();
			break;
		case PROCEDURE_CALL:
			result = ((ProcedureCall) call).getParagraph().getCtx();
			break;
		case REPORT_DESCRIPTION_ENTRY_CALL:
			result = ((ReportDescriptionEntryCall) call).getReportDescriptionEntry().getCtx();
			break;
		case SCREEN_DESCRIPTION_ENTRY_CALL:
			result = ((ScreenDescriptionEntryCall) call).getScreenDescriptionEntry().getCtx();
			break;
		case TABLE_CALL:
			result = ((TableCall) call).getDataDescriptionEntry().getCtx();
			break;
		case UNDEFINED_CALL:
			result = null;
			break;
		default:
			LOG.warn("Could not determine id for call with type {}.", callType);
			result = null;
		}

		return result;
	}

	@SuppressWarnings("unchecked")
	protected String getCallerIds(final List<?> calls) {
		return ((List<Call>) calls).stream().map(call -> idRegistry.assureRelativeId(call.getCtx(), compilationUnit))
				.collect(Collectors.joining(COMMA));
	}

	@SuppressWarnings("unchecked")
	protected String getCallerNames(final List<?> calls) {
		return ((List<Call>) calls).stream()
				.map(call -> NamingUtils.determineFullQualifiedName(call.getCtx(), compilationUnit))
				.collect(Collectors.joining(COMMA));
	}

	protected String getCallId(final Call call) {
		final ParserRuleContext calledParseTree = getCalledParseTree(call);
		final String result;

		if (calledParseTree == null) {
			result = null;
		} else {
			result = idRegistry.assureRelativeId(calledParseTree, compilationUnit);
		}

		return result;
	}

	@Override
	public Boolean visitAddStatement(final AddStatementContext ctx) {
		addStatementIdSpan(ctx);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitCdName(final CdNameContext ctx) {
		return addCall(ctx);
	}

	@Override
	public Boolean visitCobolWord(final CobolWordContext ctx) {
		final Element element = findParentDomElement(ctx).addElement(X_I);
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitCommentEntry(final CommentEntryContext ctx) {
		final Element element = findParentDomElement(ctx).addElement(X_C);
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitCompilationUnit(final CompilationUnitContext ctx) {
		final Element element = document.addElement(PRE).addAttribute(CLASS, "compilation-unit");
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitDataDescName(final DataDescNameContext ctx) {
		return addCall(ctx);
	}

	@Override
	public Boolean visitDataDescriptionEntryFormat1(final DataDescriptionEntryFormat1Context ctx) {
		final DataDescriptionEntry dataDescriptionEntry = (DataDescriptionEntry) getASGElement(ctx);
		final String ids = getCallerIds(dataDescriptionEntry.getCalls());
		final String callerNames = getCallerNames(dataDescriptionEntry.getCalls());

		final Element element = addDiv(ctx).addAttribute(CLASS, "data-description-entry")
				.addAttribute(ID, idRegistry.assureId(ctx)).addAttribute(DATA_HREFS, ids)
				.addAttribute(DATA_CALLERS, callerNames);
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitDataDescriptionEntryFormat2(final DataDescriptionEntryFormat2Context ctx) {
		final DataDescriptionEntry dataDescriptionEntry = (DataDescriptionEntry) getASGElement(ctx);
		final String ids = getCallerIds(dataDescriptionEntry.getCalls());
		final String callerNames = getCallerNames(dataDescriptionEntry.getCalls());

		final Element element = addDiv(ctx).addAttribute(CLASS, "data-description-entry")
				.addAttribute(ID, idRegistry.assureId(ctx)).addAttribute(DATA_HREFS, ids)
				.addAttribute(DATA_CALLERS, callerNames);
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitDataDescriptionEntryFormat3(final DataDescriptionEntryFormat3Context ctx) {
		final DataDescriptionEntry dataDescriptionEntry = (DataDescriptionEntry) getASGElement(ctx);
		final String ids = getCallerIds(dataDescriptionEntry.getCalls());
		final String callerNames = getCallerNames(dataDescriptionEntry.getCalls());

		final Element element = addDiv(ctx).addAttribute(CLASS, "data-description-entry")
				.addAttribute(ID, idRegistry.assureId(ctx)).addAttribute(DATA_HREFS, ids)
				.addAttribute(DATA_CALLERS, callerNames);
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitDataDivision(final DataDivisionContext ctx) {
		final Element element = addDiv(ctx).addAttribute(CLASS, "data-division");
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitDataName(final DataNameContext ctx) {
		return addCall(ctx);
	}

	@Override
	public Boolean visitDisplayStatement(final DisplayStatementContext ctx) {
		addStatementIdSpan(ctx);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitEnvironmentDivision(final EnvironmentDivisionContext ctx) {
		final Element element = addDiv(ctx).addAttribute(CLASS, "environment-division");
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitFileControlEntry(final FileControlEntryContext ctx) {
		final Element element = addDiv(ctx).addAttribute(CLASS, "file-control-entry").addAttribute(ID,
				idRegistry.assureId(ctx));
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitFileName(final FileNameContext ctx) {
		return addCall(ctx);
	}

	@Override
	public Boolean visitGobackStatement(final GobackStatementContext ctx) {
		addStatementIdSpan(ctx);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitIdentificationDivision(final IdentificationDivisionContext ctx) {
		final Element element = addDiv(ctx).addAttribute(CLASS, "identification-division");
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitIdentifier(final IdentifierContext ctx) {
		return addCall(ctx);
	}

	@Override
	public Boolean visitLiteral(final LiteralContext ctx) {
		final Element element = findParentDomElement(ctx).addElement(X_L);
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitLocalName(final LocalNameContext ctx) {
		return addCall(ctx);
	}

	@Override
	public Boolean visitMnemonicName(final MnemonicNameContext ctx) {
		return addCall(ctx);
	}

	@Override
	public Boolean visitMoveStatement(final MoveStatementContext ctx) {
		addStatementIdSpan(ctx);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitParagraph(final ParagraphContext ctx) {
		final Paragraph paragraph = (Paragraph) getASGElement(ctx);
		final String ids = getCallerIds(paragraph.getCalls());
		final String callerNames = getCallerNames(paragraph.getCalls());

		final Element element = addDiv(ctx).addAttribute(CLASS, "paragraph").addAttribute(ID, idRegistry.assureId(ctx))
				.addAttribute(DATA_HREFS, ids).addAttribute(DATA_CALLERS, callerNames);
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitPerformStatement(final PerformStatementContext ctx) {
		addStatementIdSpan(ctx);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitProcedureDivision(final ProcedureDivisionContext ctx) {
		final Element element = addDiv(ctx).addAttribute(CLASS, "procedure-division");
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitProcedureName(final ProcedureNameContext ctx) {
		return addCall(ctx);
	}

	@Override
	public Boolean visitProcedureSection(final ProcedureSectionContext ctx) {
		final Element element = addDiv(ctx).addAttribute(CLASS, "procedure-section");
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitProgramIdParagraph(final ProgramIdParagraphContext ctx) {
		addStatementIdSpan(ctx);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitProgramUnit(final ProgramUnitContext ctx) {
		final Element element = addDiv(ctx).addAttribute(CLASS, "program-unit");
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitRecordName(final RecordNameContext ctx) {
		return addCall(ctx);
	}

	@Override
	public Boolean visitReportName(final ReportNameContext ctx) {
		return addCall(ctx);
	}

	@Override
	public Boolean visitSectionName(final SectionNameContext ctx) {
		return addCall(ctx);
	}

	@Override
	public Boolean visitStopStatement(final StopStatementContext ctx) {
		addStatementIdSpan(ctx);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitSubtractStatement(final SubtractStatementContext ctx) {
		addStatementIdSpan(ctx);

		return visitChildren(ctx);
	}

	@Override
	public Boolean visitTableCall(final TableCallContext ctx) {
		return addCall(ctx);
	}

	@Override
	public Boolean visitTerminal(final TerminalNode node) {
		final Element parent = findParentDomElement(node);
		final CommonTokenStream tokens = compilationUnit.getTokens();
		final int tokPos = node.getSourceInterval().a;
		final String hiddenTokensToLeft = TokenUtils.getHiddenTokensToLeft(tokPos, tokens);

		if (hiddenTokensToLeft == null || hiddenTokensToLeft.isEmpty()) {
			parent.addText(hiddenTokensToLeft + node.getText());
		} else {
			parent.addElement(X_C).addText(hiddenTokensToLeft);
			parent.addText(node.getText());
		}

		return true;
	}

	@Override
	public Boolean visitWorkingStorageSection(final WorkingStorageSectionContext ctx) {
		final Element element = addDiv(ctx).addAttribute(CLASS, "working-storage-section");
		domElementRegistry.put(ctx, element);

		return visitChildren(ctx);
	}
}
