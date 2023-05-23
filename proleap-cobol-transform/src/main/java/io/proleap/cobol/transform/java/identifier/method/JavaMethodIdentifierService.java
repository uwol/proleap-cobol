package io.proleap.cobol.transform.java.identifier.method;

import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Section;

public interface JavaMethodIdentifierService {

	String mapToIdentifier(Paragraph paragraph);

	String mapToIdentifier(Section section);
}
