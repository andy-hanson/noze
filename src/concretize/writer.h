#pragma once

#include "../util.h"

struct Writer {
private:
	Arena& arena;
	ArrBuilder<const char> res;

public:
	inline Writer(Arena& _arena) : arena{_arena}, res{} {}

	inline void writeChar(const char c) {
		res.add(arena, c);
	}
	inline void writeStatic(const CStr text) {
		writeStr(strLiteral(text));
	}
	void writeStr(const Str s);
	void writeInt(const ssize_t s);
	void writeUint(const size_t s);

	inline const Str finish() {
		return res.finish();
	}
};
