#include "./lineAndColumnGetter.h"

#include "./arrBuilder.h"
#include "./arrUtil.h"
#include "./str.h"

namespace {
	const uint TAB_SIZE = 4; // TODO: configurable

	uint mid(uint a, uint b) {
		return (a + b) / 2;
	}

	const LineAndColumn lineAndColumnAtPosSlow(const Str text, const Pos pos) {
		uint line = 0;
		uint column = 0;
		for (const char c : slice(text, 0, pos))
			switch (c) {
				case '\n':
					line++;
					column = 0;
					break;
				case '\t':
					column += TAB_SIZE;
					break;
				default:
					column++;
					break;
			}
		return LineAndColumn{line, column};
	}

	const Bool lineAndColumnEq(const LineAndColumn a, const LineAndColumn b) {
		return _and(a.line == b.line, a.column == b.column);
	}
}

const LineAndColumnGetter lineAndColumnGetterForText(Arena& arena, const Str text) {
	ArrBuilder<const Pos> res {};
	res.add(arena, 0);
	// TODO:EACHWITHINDEX
	for (const size_t i : Range{text.size})
		if (at(text, i) == '\n')
			res.add(arena, safeSizeTToUint(i + 1));
	return LineAndColumnGetter{copyStr(arena, text), res.finish()};
}

const LineAndColumn lineAndColumnAtPos(const LineAndColumnGetter lc, const Pos pos) {
	assert(pos <= lc.text.size);

	uint lowLine = 0; // inclusive
	uint highLine = safeSizeTToUint(lc.lineToPos.size); // exclusive

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
	assert(pos >= lineStart && (line == lc.lineToPos.size - 1 || pos <= at(lc.lineToPos, line + 1)));
	// Need to walk the line looking for tabs, which count as more
	// (TODO: precalculate the # tabs on each line)
	uint column= 0;
	for (const char c : sliceFromTo(lc.text, lineStart, pos))
		switch (c) {
			case '\n':
				unreachable<void>();
				break;
			case '\t':
				column += TAB_SIZE;
				break;
			default:
				column += 1;
				break;
		}

	const LineAndColumn res = LineAndColumn{line, column};
	assert(lineAndColumnEq(res, lineAndColumnAtPosSlow(lc.text, pos)));
	return res;
}
