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

void newline(WriterWithIndent& writer) {
	writeChar(writer, '\n');
	for (__attribute__((unused)) const size_t _ : Range{writer._indent})
		writeChar(writer, '\t');
}

void writeHyperlink(Writer& writer, const Str url, const Str text) {
	// documentation: https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda
	// https://purpleidea.com/blog/2018/06/29/hyperlinks-in-gnome-terminal/
	// TODO: I haven't got this to work on any terminal emulator I have installed. :(
	Arena temp {};
	if (false) {
		writeStatic(writer, "\x1b]8;;");
		writeStr(writer, url);
		writeStatic(writer, "\x1b\\");
		writeStr(writer, text);
		writeStatic(writer, "\x1b]8;;\x1b\\");
	} else {
		writeStr(writer, text);
	}
}
