#include "./writer.h"

void writeStr(Writer& writer, const Str s) {
	for (const char c : s)
		writer.res.add(writer.arena, c);
}

void writeNat(Writer& writer, const size_t s) {
	if (s >= 10)
		writeNat(writer, s / 10);
	writeChar(writer, '0' + s % 10);
}

void writeInt(Writer& writer, const Int64 i) {
	if (i < 0)
		writeChar(writer, '-');
	writeNat(writer, i < 0 ? -i : i);
}

void writeEscapedChar(Writer& writer, const char c) {
	switch (c) {
		case '\n':
			writeStatic(writer, "\\n");
			break;
		case '\t':
			writeStatic(writer, "\\t");
			break;
		case '\'':
			writeStatic(writer, "\\\'");
			break;
		case '"':
			writeStatic(writer, "\\\"");
			break;
		case '\\':
			writeStatic(writer, "\\\\");
			break;
		case '\0':
			writeStatic(writer, "\\0");
			break;
		default:
			writeChar(writer, c);
			break;
	}
}
