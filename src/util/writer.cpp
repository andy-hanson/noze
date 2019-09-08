#include "./writer.h"

void writeStr(Writer* writer, const Str s) {
	for (const char c : s)
		add<const char>(writer->arena, &writer->res, c);
}

void writeNat(Writer* writer, const Nat64 n) {
	if (n.value >= 10)
		writeNat(writer, n.value / 10);
	writeChar(writer, '0' + n.value % 10);
}

void writeInt(Writer* writer, const Int64 i) {
	if (i.value < 0)
		writeChar(writer, '-');
	writeNat(writer, Nat64{static_cast<uint64_t>(i.value < 0 ? -i.value : i.value)});
}

void writeEscapedChar(Writer* writer, const char c) {
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

void newline(WriterWithIndent* writer) {
	writeChar(writer, '\n');
	for (__attribute__((unused)) const size_t _ : Range{writer->_indent})
		writeChar(writer, '\t');
}

void writeHyperlink(Writer* writer, const Str url, const Str text) {
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
