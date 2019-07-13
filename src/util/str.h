#pragma once

#include "./arena.h"
#include "./arr.h"
#include "./bool.h"
#include "./comparison.h"

struct NulTerminatedStr {
	const Str str;
	explicit inline NulTerminatedStr(const Str _str) : str{_str} {
		assert(at(str, size(str) - 1) == '\0');
	}

	inline CStr asCStr() const {
		return str.begin();
	}
};

inline constexpr CStr end(const CStr c) {
	return *c == '\0' ? c : end(c + 1);
}

inline constexpr const Str strLiteral(const CStr c) {
	return Str{c, static_cast<size_t>(end(c) - c)};
}

inline const NulTerminatedStr nulTerminatedStrLiteral(const CStr c) {
	return NulTerminatedStr{Str{c, static_cast<size_t>(end(c) + 1 - c)}};
}

const NulTerminatedStr strToNulTerminatedStr(Arena* arena, const Str s);

inline CStr strToCStr(Arena* arena, const Str s) {
	return strToNulTerminatedStr(arena, s).asCStr();
}

const Bool strEq(const Str a, const Str b);

Comparison compareStr(const Str a, const Str b);

inline const Bool strEqLiteral(const Str s, const CStr b) {
	return strEq(s, strLiteral(b));
}

const Str copyStr(Arena* arena, const Str s);

inline const NulTerminatedStr copyNulTerminatedStr(Arena* arena, const NulTerminatedStr in) {
	return NulTerminatedStr{copyStr(arena, in.str)};
}

const Str stripNulTerminator(const NulTerminatedStr n);
