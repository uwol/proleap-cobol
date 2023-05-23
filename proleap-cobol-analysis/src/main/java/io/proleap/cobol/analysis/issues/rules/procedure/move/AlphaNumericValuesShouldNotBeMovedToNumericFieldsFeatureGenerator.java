package io.proleap.cobol.analysis.issues.rules.procedure.move;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.proleap.cobol.CobolBaseVisitor;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.analysis.issues.rules.FeatureGenerator;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.call.Call;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveStatement;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveToSendingArea;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveToStatement;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.commons.type.CobolTypeEnum;
import io.proleap.cobol.commons.type.CobolTypeService;

@Singleton
public class AlphaNumericValuesShouldNotBeMovedToNumericFieldsFeatureGenerator extends FeatureGenerator<MoveStatement> {

	@Inject
	private CobolTypeService cobolTypeService;

	@Override
	public Stream<MoveStatement> getAll(final CompilationUnit compilationUnit) {
		final Program program = compilationUnit.getProgram();
		final List<MoveStatement> result = new ArrayList<>();

		final CobolBaseVisitor<Boolean> visitor = new CobolBaseVisitor<Boolean>() {
			@Override
			public Boolean visitMoveStatement(final CobolParser.MoveStatementContext ctx) {
				final MoveStatement moveStatement = (MoveStatement) program.getASGElementRegistry().getASGElement(ctx);
				if (moveStatement != null) {
					if (isRelevantMoveStatement(moveStatement)) {
						result.add(moveStatement);
					}
				}
				return true;
			}
		};

		visitor.visit(compilationUnit.getCtx());

		if (result.isEmpty()) {
			return Stream.empty();
		} else {
			return result.stream();
		}
	}

	protected boolean isRelevantMoveStatement(final MoveStatement moveStatement) {
		final boolean result;
		final MoveToStatement moveToStatement = moveStatement.getMoveToStatement();
		final Set<CobolTypeEnum> typeSet = new HashSet<>();

		if (moveToStatement == null) {
			result = false;
		} else {
			final MoveToSendingArea sendingArea = moveToStatement.getSendingArea();
			final ValueStmt sendingAreaValueStmt = sendingArea.getSendingAreaValueStmt();
			final CobolTypeEnum sendingType = cobolTypeService.getType(sendingAreaValueStmt);

			if (sendingType != null) {
				final List<Call> receivingAreaCalls = moveToStatement.getReceivingAreaCalls();

				for (final Call call : receivingAreaCalls) {
					final CobolTypeEnum type = cobolTypeService.getType(call);
					typeSet.add(type);
				}

				if (typeSet.contains(CobolTypeEnum.INTEGER) || typeSet.contains(CobolTypeEnum.FLOAT)) {
					if (CobolTypeEnum.STRING.equals(sendingType)) {
						result = true;
					} else {
						result = false;
					}
				} else {
					result = false;
				}
			} else {
				result = false;
			}
		}

		return result;
	}
}
