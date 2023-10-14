package io.proleap.cobol.commons.datadescription.impl;

import java.util.regex.Matcher;

import jakarta.inject.Singleton;

import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntryGroup;
import io.proleap.cobol.asg.metamodel.data.datadescription.PictureClause;
import io.proleap.cobol.commons.datadescription.CobolPictureLengthService;
import io.proleap.cobol.commons.util.CobolPictureParseUtils;

@Singleton
public class CobolPictureLengthServiceImpl implements CobolPictureLengthService {

	@Override
	public Integer getFractionalPartLength(final String pictureString) {
		final Matcher matcher9 = CobolPictureParseUtils.PATTERN_9.matcher(pictureString);
		final Matcher matcher9Length = CobolPictureParseUtils.PATTERN_9Length.matcher(pictureString);
		final Matcher matcher9DOT9 = CobolPictureParseUtils.PATTERN_9DOT9.matcher(pictureString);
		final Matcher matcher9V9 = CobolPictureParseUtils.PATTERN_9V9.matcher(pictureString);
		final Matcher matcherS9 = CobolPictureParseUtils.PATTERN_S9.matcher(pictureString);
		final Matcher matcherS9Length = CobolPictureParseUtils.PATTERN_S9Length.matcher(pictureString);
		final Matcher matcher9LengthV9 = CobolPictureParseUtils.PATTERN_9LengthV9.matcher(pictureString);
		final Matcher matcher9LengthV9Length = CobolPictureParseUtils.PATTERN_9LengthV9Length.matcher(pictureString);
		final Matcher matcher9V9Length = CobolPictureParseUtils.PATTERN_9V9Length.matcher(pictureString);

		final Integer result;

		if (matcher9.matches()) {
			result = 0;
		} else if (matcher9Length.matches()) {
			result = 0;
		} else if (matcher9DOT9.matches()) {
			result = matcher9DOT9.group(2).length();
		} else if (matcher9V9.matches()) {
			result = matcher9V9.group(2).length();
		} else if (matcherS9.matches()) {
			result = 0;
		} else if (matcherS9Length.matches()) {
			result = 0;
		} else if (matcher9LengthV9.matches()) {
			result = matcher9LengthV9.group(2).length();
		} else if (matcher9LengthV9Length.matches()) {
			result = Integer.valueOf(matcher9LengthV9Length.group(2));
		} else if (matcher9V9Length.matches()) {
			result = Integer.valueOf(matcher9V9Length.group(2));
		} else {
			result = null;
		}

		return result;
	}

	@Override
	public Integer getIntegerPartLength(final String pictureString) {
		final Matcher matcher9 = CobolPictureParseUtils.PATTERN_9.matcher(pictureString);
		final Matcher matcher9Length = CobolPictureParseUtils.PATTERN_9Length.matcher(pictureString);
		final Matcher matcher9DOT9 = CobolPictureParseUtils.PATTERN_9DOT9.matcher(pictureString);
		final Matcher matcher9V9 = CobolPictureParseUtils.PATTERN_9V9.matcher(pictureString);
		final Matcher matcherS9 = CobolPictureParseUtils.PATTERN_S9.matcher(pictureString);
		final Matcher matcherS9Length = CobolPictureParseUtils.PATTERN_S9Length.matcher(pictureString);
		final Matcher matcher9LengthV9 = CobolPictureParseUtils.PATTERN_9LengthV9.matcher(pictureString);
		final Matcher matcher9LengthV9Length = CobolPictureParseUtils.PATTERN_9LengthV9Length.matcher(pictureString);
		final Matcher matcher9V9Length = CobolPictureParseUtils.PATTERN_9V9Length.matcher(pictureString);

		final Integer result;

		if (matcher9.matches()) {
			result = matcher9.group(0).length();
		} else if (matcher9Length.matches()) {
			result = Integer.valueOf(matcher9Length.group(1));
		} else if (matcher9DOT9.matches()) {
			result = matcher9DOT9.group(1).length();
		} else if (matcher9V9.matches()) {
			result = matcher9V9.group(1).length();
		} else if (matcherS9.matches()) {
			result = matcherS9.group(0).length();
		} else if (matcherS9Length.matches()) {
			result = Integer.valueOf(matcherS9Length.group(1));
		} else if (matcher9LengthV9.matches()) {
			result = Integer.valueOf(matcher9LengthV9.group(1));
		} else if (matcher9LengthV9Length.matches()) {
			result = Integer.valueOf(matcher9LengthV9Length.group(1));
		} else if (matcher9V9Length.matches()) {
			result = matcher9V9Length.group(1).length();
		} else {
			result = null;
		}

		return result;
	}

	@Override
	public Integer getLength(final DataDescriptionEntry dataDescriptionEntry) {
		final Integer result;

		switch (dataDescriptionEntry.getDataDescriptionEntryType()) {
		case GROUP:
		case SCALAR:
			final DataDescriptionEntryGroup dataDescriptionEntryGroup = (DataDescriptionEntryGroup) dataDescriptionEntry;
			final PictureClause pictureClause = dataDescriptionEntryGroup.getPictureClause();
			final String pictureString = pictureClause == null ? null : pictureClause.getPictureString();
			return pictureString == null ? null : getLength(pictureString);
		default:
			result = null;
		}

		return result;
	}

	@Override
	public Integer getLength(final String pictureString) {
		final Matcher matcher9 = CobolPictureParseUtils.PATTERN_9.matcher(pictureString);
		final Matcher matcher9Length = CobolPictureParseUtils.PATTERN_9Length.matcher(pictureString);
		final Matcher matcher9DOT9 = CobolPictureParseUtils.PATTERN_9DOT9.matcher(pictureString);
		final Matcher matcher9V9 = CobolPictureParseUtils.PATTERN_9V9.matcher(pictureString);
		final Matcher matcherS9 = CobolPictureParseUtils.PATTERN_S9.matcher(pictureString);
		final Matcher matcherS9Length = CobolPictureParseUtils.PATTERN_S9Length.matcher(pictureString);
		final Matcher matcher9LengthV9 = CobolPictureParseUtils.PATTERN_9LengthV9.matcher(pictureString);
		final Matcher matcher9LengthV9Length = CobolPictureParseUtils.PATTERN_9LengthV9Length.matcher(pictureString);
		final Matcher matcher9V9Length = CobolPictureParseUtils.PATTERN_9V9Length.matcher(pictureString);
		final Matcher matcherA = CobolPictureParseUtils.PATTERN_A.matcher(pictureString);
		final Matcher matcherALength = CobolPictureParseUtils.PATTERN_ALength.matcher(pictureString);
		final Matcher matcherX = CobolPictureParseUtils.PATTERN_X.matcher(pictureString);
		final Matcher matcherXLength = CobolPictureParseUtils.PATTERN_XLength.matcher(pictureString);

		final Integer result;

		if (matcher9.matches()) {
			result = matcher9.group(0).length();
		} else if (matcher9Length.matches()) {
			result = Integer.valueOf(matcher9Length.group(1));
		} else if (matcher9DOT9.matches()) {
			result = matcher9DOT9.group(1).length() + 1 + matcher9DOT9.group(2).length();
		} else if (matcher9V9.matches()) {
			result = matcher9V9.group(1).length() + 1 + matcher9V9.group(2).length();
		} else if (matcherS9.matches()) {
			result = 1 + matcherS9.group(0).length();
		} else if (matcherS9Length.matches()) {
			result = 1 + Integer.valueOf(matcherS9Length.group(1));
		} else if (matcher9LengthV9.matches()) {
			result = Integer.valueOf(matcher9LengthV9.group(1)) + 1 + matcher9V9.group(2).length();
		} else if (matcher9LengthV9Length.matches()) {
			result = Integer.valueOf(matcher9LengthV9Length.group(1))
					+ Integer.valueOf(matcher9LengthV9Length.group(2));
		} else if (matcher9V9Length.matches()) {
			result = matcher9V9Length.group(1).length() + 1 + Integer.valueOf(matcher9LengthV9Length.group(2));
		} else if (matcherA.matches()) {
			result = matcherA.group(0).length();
		} else if (matcherALength.matches()) {
			result = Integer.valueOf(matcherALength.group(1));
		} else if (matcherX.matches()) {
			result = matcherX.group(0).length();
		} else if (matcherXLength.matches()) {
			result = Integer.valueOf(matcherXLength.group(1));
		} else {
			result = null;
		}

		return result;
	}

	@Override
	public Integer getStringLength(final String pictureString) {
		final Matcher matcherA = CobolPictureParseUtils.PATTERN_A.matcher(pictureString);
		final Matcher matcherALength = CobolPictureParseUtils.PATTERN_ALength.matcher(pictureString);
		final Matcher matcherX = CobolPictureParseUtils.PATTERN_X.matcher(pictureString);
		final Matcher matcherXLength = CobolPictureParseUtils.PATTERN_XLength.matcher(pictureString);

		final Integer result;

		if (matcherA.matches()) {
			result = matcherA.group(0).length();
		} else if (matcherALength.matches()) {
			result = Integer.valueOf(matcherALength.group(1));
		} else if (matcherX.matches()) {
			result = matcherX.group(0).length();
		} else if (matcherXLength.matches()) {
			result = Integer.valueOf(matcherXLength.group(1));
		} else {
			result = null;
		}

		return result;
	}
}
