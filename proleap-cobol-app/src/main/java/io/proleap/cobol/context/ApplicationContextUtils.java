package io.proleap.cobol.context;

import org.slf4j.LoggerFactory;

import io.micronaut.context.ApplicationContext;
import io.proleap.cobol.interpreter.context.PostConstructBean;

public class ApplicationContextUtils {

	private static ApplicationContext applicationContext;

	/**
	 * cf. double checked locking singleton -> this is a best practice!
	 */
	private static ApplicationContext assureApplicationContext() {
		if (applicationContext == null) {
			synchronized (ApplicationContextUtils.class) {
				if (applicationContext == null) {
					applicationContext = createApplicationContext();
				}
			}
		}

		return applicationContext;
	}

	private static ApplicationContext createApplicationContext() {
		final long startTime = System.currentTimeMillis();

		final ApplicationContext result = ApplicationContext.run();

		/*
		 * PostConstructBeans have to be retrieved from the application context here, so
		 * that otherwise (due to Micronaut) lazy @PostConstruct is called eagerly
		 */
		result.getBeansOfType(PostConstructBean.class);

		final long stopTime = System.currentTimeMillis();
		final long elapsedTime = stopTime - startTime;

		LoggerFactory.getLogger(ApplicationContextUtils.class).info("Application context created in {} ms",
				elapsedTime);

		return result;
	}

	public static <T> T getBean(final Class<T> beanType) {
		return assureApplicationContext().getBean(beanType);
	}

	public static <T> void inject(final T instance) {
		assureApplicationContext().inject(instance);
	}

	public static <T> void warmup() {
		LoggerFactory.getLogger(ApplicationContextUtils.class).info("Prefetched {} beans",
				assureApplicationContext().getBeansOfType(Object.class).size());
	}
}
