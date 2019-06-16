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
		writer.writeChar('_');
		writer.writeStr(name);
	} else
		for (const char c : name)
			switch (c) {
				case '-':
					writer.writeChar('_');
					break;
				case '?':
					writer.writeStatic("__q");
					break;
				case '+':
					writer.writeStatic("plus");
					break;
				case '*':
					writer.writeStatic("times");
					break;
				case '/':
					writer.writeStatic("div");
					break;
				case '<':
					writer.writeStatic("less");
					break;
				case '>':
					writer.writeStatic("greater");
					break;
				case '=':
					writer.writeStatic("equal");
					break;
				default:
					writer.writeChar(c);
					break;
			}
}

