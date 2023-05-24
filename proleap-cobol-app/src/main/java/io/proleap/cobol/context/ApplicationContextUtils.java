package io.proleap.cobol.context;

import org.slf4j.LoggerFactory;

import io.micronaut.context.ApplicationContext;
import io.proleap.cobol.interpreter.context.PostConstructBean;

public class ApplicationContextUtils {

	private static ApplicationContext APPLICATION_CONTEXT;

	/**
	 * cf. double checked locking singleton -> this is a best practice!
	 */
	private static ApplicationContext assureApplicationContext() {
		if (APPLICATION_CONTEXT == null) {
			synchronized (ApplicationContextUtils.class) {
				if (APPLICATION_CONTEXT == null) {
					APPLICATION_CONTEXT = createApplicationContext();
				}
			}
		}

		return APPLICATION_CONTEXT;
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
