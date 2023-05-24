package io.proleap.cobol.service.input.git;

import java.security.cert.X509Certificate;

import javax.net.ssl.X509TrustManager;

public class BonnaFideTrustManager implements X509TrustManager {

	@Override
	public void checkClientTrusted(final X509Certificate[] certs, final String authType) {
	}

	@Override
	public void checkServerTrusted(final X509Certificate[] certs, final String authType) {
	}

	@Override
	public X509Certificate[] getAcceptedIssuers() {
		return null;
	}
}