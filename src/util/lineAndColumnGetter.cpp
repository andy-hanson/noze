#include "./lineAndColumnGetter.h"

#include "./arrBuilder.h"
#include "./arrUtil.h"
#include "./str.h"

namespace {
	const uint TAB_SIZE = 4; // TODO: configurable

	uint mid(uint a, uint b) {
		return (a + b) / 2;
	}

	uint8_t getNTabs(const Str text) {
		uint8_t i = 0;
		while (i < MAX_UINT8
			&& i < size(text)
			&& at(text, i) == '\t') {
			i++;
		}
		return i;
	}
}

const LineAndColumnGetter lineAndColumnGetterForText(Arena* arena, const Str text) {
	ArrBuilder<const Pos> lineToPos {};
	ArrBuilder<const uint8_t> lineToNTabs {};

	add<const Pos>(arena, &lineToPos, 0);
	add<const uint8_t>(arena, &lineToNTabs, getNTabs(text));

	for (const size_t i : indices(text))
		if (at(text, i) == '\n') {
			add<const Pos>(arena, &lineToPos, safeSizeTToUint(i + 1));
			add<const uint8_t>(arena, &lineToNTabs, getNTabs(slice(text, i + 1)));
		}
	return LineAndColumnGetter{finishArr(&lineToPos), finishArr(&lineToNTabs)};
}

const LineAndColumn lineAndColumnAtPos(const LineAndColumnGetter lc, const Pos pos) {
	uint lowLine = 0; // inclusive
	uint highLine = safeSizeTToUint(size(lc.lineToPos)); // exclusive

	while (lowLine < highLine - 1) {
		const uint middleLine = mid(lowLine, highLine);
		const uint middlePos = at(lc.lineToPos, middleLine);
		if (pos == middlePos)
			return LineAndColumn{middleLine, 0};
		else if (pos < middlePos)
			// Exclusive -- must be on a previous line
			highLine = middleLine;
		else
			// Inclusive -- may be on a later character of the same line
			lowLine = middleLine;
	}

	const uint line = lowLine;
	const Pos lineStart = at(lc.lineToPos, line);
	assert(pos >= lineStart && (line == size(lc.lineToPos) - 1 || pos <= at(lc.lineToPos, line + 1)));

	const uint nCharsIntoLine = pos - lineStart;
	const uint8_t nTabs = at(lc.lineToNTabs, line);
	const uint column = nCharsIntoLine <= nTabs
		? nCharsIntoLine * TAB_SIZE
		: nTabs * (TAB_SIZE - 1) + nCharsIntoLine;
	return LineAndColumn{line, column};
}
