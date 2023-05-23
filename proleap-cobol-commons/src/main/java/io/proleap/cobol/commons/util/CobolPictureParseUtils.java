package io.proleap.cobol.commons.util;

import java.util.regex.Pattern;

public class CobolPictureParseUtils {

	public static Pattern PATTERN_9 = Pattern.compile("[9]+");

	public static Pattern PATTERN_9DOT9 = Pattern.compile("([9]+).([9]+)");

	public static Pattern PATTERN_9Length = Pattern.compile("9\\(([0-9]+)\\)");

	public static Pattern PATTERN_9LengthV9 = Pattern.compile("9\\(([0-9]+)\\)V([9]+)");

	public static Pattern PATTERN_9LengthV9Length = Pattern.compile("9\\(([0-9]+)\\)V9\\(([0-9]+)\\)");

	public static Pattern PATTERN_9V9 = Pattern.compile("([9]+)V([9]+)");

	public static Pattern PATTERN_9V9Length = Pattern.compile("([9]+)V9\\(([0-9]+)\\)");

	public static Pattern PATTERN_A = Pattern.compile("[A]+");

	public static Pattern PATTERN_ALength = Pattern.compile("A\\(([0-9]+)\\)");

	public static Pattern PATTERN_S9 = Pattern.compile("S[9]+");

	public static Pattern PATTERN_S9Length = Pattern.compile("S9\\(([0-9]+)\\)");

	public static Pattern PATTERN_S9LengthV9 = Pattern.compile("S9\\(([0-9]+)\\)V([9]+)");

	public static Pattern PATTERN_S9LengthV9Length = Pattern.compile("S9\\(([0-9]+)\\)V9\\(([0-9]+)\\)");

	public static Pattern PATTERN_S9V9 = Pattern.compile("S[9]+V[9]+");

	public static Pattern PATTERN_S9V9Length = Pattern.compile("S([9]+)V9\\(([0-9]+)\\)");

	public static Pattern PATTERN_X = Pattern.compile("[X]+");

	public static Pattern PATTERN_XLength = Pattern.compile("X\\(([0-9]+)\\)");
}
