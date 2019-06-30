#pragma once

#include "./arrBuilder.h"
#include "./str.h"

struct Writer {
	Arena& arena;
	ArrBuilder<const char> res {};
};

inline const Str finishWriter(Writer& writer) {
	return writer.res.finish();
}

inline CStr finishWriterToCStr(Writer& writer) {
	writer.res.add(writer.arena, '\0');
	return writer.res.finish().begin();
}

//TODO:KILL
inline void writeChar(Writer& writer, const char c) {
	writer.res.add(writer.arena, c);
}
void writeStr(Writer& writer, const Str s);
inline void writeStatic(Writer& writer, const char* c) {
	writeStr(writer, strLiteral(c));
}
void writeNat(Writer& writer, const Nat64 s);
void writeInt(Writer& writer, const Int64 s);
inline void writeBool(Writer& writer, const Bool b) {
	writeStatic(writer, b ? "true" : "false");
}

template <typename T, typename Cb>
void writeWithCommas(Writer& writer, const Arr<T> a, Cb cb) {
	for (const size_t i : Range{a.size}) {
		if (i != 0)
			writeStatic(writer, ", ");
		cb(at(a, i));
	}
}

template <typename T, typename Cb>
void writeWithCommas(Writer& writer, const Arr<T> a, const Bool leadingComma, Cb cb) {
	for (const size_t i : Range{a.size}) {
		if (leadingComma || i != 0)
			writeStatic(writer, ", ");
		cb(at(a, i));
	}
}

void writeEscapedChar(Writer& writer, const char c);

struct WriterWithIndent {
	Writer& writer;
	uint _indent = 0;
};

void newline(WriterWithIndent& writer);

inline void decrIndent(WriterWithIndent& writer) {
	assert(writer._indent != 0);
	writer._indent--;
}

inline void incrIndent(WriterWithIndent& writer) {
	writer._indent++;
}

inline void indent(WriterWithIndent& writer) {
	incrIndent(writer);
	newline(writer);
}

inline void dedent(WriterWithIndent& writer) {
	decrIndent(writer);
	newline(writer);
}

inline void writeChar(WriterWithIndent& writer, const char c) {
	writeChar(writer.writer, c);
}

inline void writeStatic(WriterWithIndent& writer, const CStr text) {
	writeStatic(writer.writer, text);
}

inline void writeStr(WriterWithIndent& writer, const Str s) {
	writeStr(writer.writer, s);
}
