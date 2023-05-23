package io.proleap.env.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import javax.inject.Singleton;

import io.proleap.env.EnvService;

@Singleton
public class SystemEnvServiceImpl implements EnvService {

	private final Map<String, String> cache = new HashMap<>();

	@Override
	public String getEnv(final String key, final String fallback) {
		Objects.requireNonNull(key);
		Objects.requireNonNull(fallback);

		if (cache.get(key) == null) {
			final String envValue = System.getenv(key);

			if (envValue != null) {
				cache.put(key, envValue);
			} else {
				cache.put(key, fallback);
			}
		}

		return cache.get(key);
	}
}
