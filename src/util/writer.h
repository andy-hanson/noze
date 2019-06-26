#pragma once

#include "../util.h"

struct Writer {
	Arena& arena;
	ArrBuilder<const char> res;

	inline Writer(Arena& _arena) : arena{_arena}, res{} {}

	inline const Str finish() {
		return res.finish();
	}

	inline CStr finishCStr() {
		res.add(arena, '\0');
		return res.finish().begin();
	}
};

//TODO:KILL
inline void writeChar(Writer& writer, const char c) {
	writer.res.add(writer.arena, c);
}
void writeStr(Writer& writer, const Str s);
inline void writeStatic(Writer& writer, const char* c) {
	writeStr(writer, strLiteral(c));
}
void writeNat(Writer& writer, const size_t s);
void writeInt(Writer& writer, const ssize_t s);
inline void writeBool(Writer& writer, const Bool b) {
	writeStatic(writer, b ? "true" : "false");
}

template <typename T, typename Cb>
void writeWithCommas(Writer& writer, const Arr<T> a, Cb cb) {
	for (const size_t i : Range{a.size}) {
		if (i != 0)
			writeStatic(writer, ", ");
		cb(a[i]);
	}
}

template <typename T, typename Cb>
void writeWithCommas(Writer& writer, const Arr<T> a, const Bool leadingComma, Cb cb) {
	for (const size_t i : Range{a.size}) {
		if (leadingComma || i != 0)
			writeStatic(writer, ", ");
		cb(a[i]);
	}
}

void writeEscapedChar(Writer& writer, const char c);
