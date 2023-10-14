package io.proleap.cobol.transform.java.identifier.impl;

import jakarta.inject.Singleton;

import io.proleap.cobol.transform.java.identifier.JavaIdentifierService;

@Singleton
public class JavaIdentifierServiceImpl implements JavaIdentifierService {

	@Override
	public String mapToIdentifier(final String identifier) {
		final String result;

		if (identifier == null) {
			result = null;
		} else {
			final String identifierReplaced = identifier.replace('#', '$');
			final String identifierCleaned = identifierReplaced.toLowerCase().replace("-", "_");

			if (identifierCleaned == null || identifierCleaned.isEmpty()) {
				result = "";
			} else {
				result = io.proleap.cobol.asg.util.StringUtils.lowercaseFirstLetter(identifierCleaned);
			}
		}

		return result;
	}
}
