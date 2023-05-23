package io.proleap.service.mail;

public interface DevOpsMailService {

	boolean sendDevOpsMessage(String subject, String text);

	boolean sendExceptionMessage(Exception e);
}