#include "./mangleName.h"

namespace {
	const Bool needsEscape(const Str name) {
		return _or3(
			strEqLiteral(name, "float"),
			strEqLiteral(name, "int"),
			strEqLiteral(name, "void"));
	}
}

const Str mangleName(Arena& arena, const Str declName) {
	Writer writer { arena };
	writeMangledName(writer, declName);
	return writer.finish();
}

void writeMangledName(Writer& writer, const Str name) {
	if (needsEscape(name)) {
		writeChar(writer, '_');
		writeStr(writer, name);
	} else
		for (const char c : name)
			switch (c) {
				case '-':
					writeChar(writer, '_');
					break;
				case '?':
					writeStatic(writer, "__q");
					break;
				case '+':
					writeStatic(writer, "plus");
					break;
				case '*':
					writeStatic(writer, "times");
					break;
				case '/':
					writeStatic(writer, "div");
					break;
				case '<':
					writeStatic(writer, "less");
					break;
				case '>':
					writeStatic(writer, "greater");
					break;
				case '=':
					writeStatic(writer, "equal");
					break;
				default:
					writeChar(writer, c);
					break;
			}
}

