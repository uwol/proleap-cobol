package io.proleap.cobol.interpreter.domain.address.impl;

import java.util.ArrayList;
import java.util.List;

import io.proleap.cobol.interpreter.domain.address.CobolAddress;
import io.proleap.cobol.interpreter.domain.address.CobolAddressGroup;

public class CobolAddressGroupImpl implements CobolAddressGroup {

	private final List<CobolAddress> addresses = new ArrayList<>();

	@Override
	public List<CobolAddress> getAddresses() {
		return addresses;
	}

	@Override
	public String toString() {
		return String.valueOf(addresses);
	}
}
