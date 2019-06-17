#pragma once

using Pos = unsigned int;

struct SourceRange {
	const Pos start;
	const Pos end;

	static inline SourceRange empty() {
		return SourceRange{0, 0};
	}
};
