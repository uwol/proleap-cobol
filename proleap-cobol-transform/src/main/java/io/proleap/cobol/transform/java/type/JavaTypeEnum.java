package io.proleap.cobol.transform.java.type;

public enum JavaTypeEnum {
	BIGDECIMAL("BigDecimal"), BOOLEAN("Boolean"), BYTE("Byte"), DATE("java.util.Date"), DOUBLE("Double"),
	INTEGER("Integer"), LONG("Long"), NUMBER("Number"), OBJECT("Object"), SINGLE("Double"), STRING("String");

	protected final String name;

	private JavaTypeEnum(final String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	@Override
	public String toString() {
		return name;
	}
}
