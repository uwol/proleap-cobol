package io.proleap.cobol.api.data;

import java.math.BigDecimal;

public interface EntityService {

	void assignTo(Object entity, Object value);

	BigDecimal getAddress(Object entity);

	BigDecimal getLength(Object entity);

	boolean isEmpty(Object entity);
}
