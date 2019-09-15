#pragma once

#include "./arena.h"
#include "./sourceRange.h"
#include "./str.h"

// This is 0-indexed, though when we write it out it will be 1-indexed
struct LineAndColumn {
	const uint line;
	const uint column;
};

struct PosAndNTabs {
	const Pos pos;
	// We need to know the number of tabs a line starts with
	// because that effects the column count.
	// (Tabs aren't allowed anywhere but the beginning of a line.)
	const uint8_t nTabs;
};

struct LineAndColumnGetter {
	const Arr<const Pos> lineToPos;
	const Arr<const uint8_t> lineToNTabs;

	inline LineAndColumnGetter(const Arr<const Pos> _lineToPos, const Arr<const uint8_t> _lineToNTabs)
		: lineToPos{_lineToPos}, lineToNTabs{_lineToNTabs} {
		assert(size(lineToPos) == size(lineToNTabs));
	}
};

// Note: 'text' is only temporary and can be freed after this
const LineAndColumnGetter lineAndColumnGetterForText(Arena* arena, const Str text);
inline const LineAndColumnGetter lineAndColumnGetterForEmptyFile(Arena* arena) {
	return lineAndColumnGetterForText(arena, emptyStr);
}

const LineAndColumn lineAndColumnAtPos(const LineAndColumnGetter lc, const Pos pos);
