#pragma once

#include "./arena.h"
#include "./sourceRange.h"
#include "./str.h"

struct LineAndColumn {
	const uint line;
	const uint column;
};

struct LineAndColumnGetter {
	const Str text; // TODO: KILL
	const Arr<const Pos> lineToPos;
};

// Note: 'text' is only temporary and can be freed after this
const LineAndColumnGetter lineAndColumnGetterForText(Arena* arena, const Str text);

const LineAndColumn lineAndColumnAtPos(const LineAndColumnGetter lc, const Pos pos);
