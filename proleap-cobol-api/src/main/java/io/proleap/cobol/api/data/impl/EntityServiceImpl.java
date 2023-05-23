package io.proleap.cobol.api.data.impl;

import java.math.BigDecimal;

import io.proleap.cobol.api.data.EntityService;

public class EntityServiceImpl implements EntityService {

	@Override
	public void assignTo(final Object entity, final Object value) {
	}

	@Override
	public BigDecimal getAddress(final Object entity) {
		return null;
	}

	@Override
	public BigDecimal getLength(final Object entity) {
		return null;
	}

	@Override
	public boolean isEmpty(final Object entity) {
		return false;
	}
}
