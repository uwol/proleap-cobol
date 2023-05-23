package io.proleap.env;

public interface EnvService {

	String getEnv(String key, String fallback);
}
