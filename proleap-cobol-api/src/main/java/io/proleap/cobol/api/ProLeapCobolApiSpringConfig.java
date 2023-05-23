package io.proleap.cobol.api;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import io.proleap.cobol.api.data.EntityService;
import io.proleap.cobol.api.data.impl.EntityServiceImpl;
import io.proleap.cobol.api.environment.inputoutput.filecontrol.FileControlService;
import io.proleap.cobol.api.environment.inputoutput.filecontrol.impl.FileControlServiceImpl;

@Configuration
public class ProLeapCobolApiSpringConfig {

	@Bean
	protected EntityService entityService() {
		return new EntityServiceImpl();
	}

	@Bean
	protected FileControlService fileControlService() {
		return new FileControlServiceImpl();
	}
}
