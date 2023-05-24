package io.proleap.cobol.env;

public interface EnvService {

	String getEnv(String key, String fallback);
}
