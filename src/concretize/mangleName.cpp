#include "./mangleName.h"

#include "../util/arrUtil.h"

namespace {
	const Bool conflictsWithCName(const Sym name) {
		switch (name.value) {
			case shortSymAlphaLiteralValue("float"):
			case shortSymAlphaLiteralValue("int"):
			case shortSymAlphaLiteralValue("void"):
				return True;
			default:
				// avoid conflicting with c's "atomic_bool" type
				return symEqLongAlphaLiteral(name, "atomic-bool");
		}
	}

	const Bool isMangledChar(const char c) {
		return _or4(
			_and('a' <= c, c <= 'z'),
			_and('A' <= c, c <= 'Z'),
			_and('0' <= c, c <= '9'),
			c == '_');
	}
}

const Str mangleName(Arena* arena, const Sym name) {
	Writer writer { arena };
	writeMangledName(&writer, name);
	const Str res = finishWriter(&writer);
	assert(isMangledName(res));
	return res;
}

void writeMangledName(Writer* writer, const Sym name) {
	Arena tempArena {};

	if (isSymOperator(name)) {
		writeStatic(writer, "_op");
		eachCharInSym(name, [&](const char c) {
			switch (c) {
				case '-':
					writeStatic(writer, "_minus");
					break;
				case '+':
					writeStatic(writer, "_plus");
					break;
				case '*':
					writeStatic(writer, "_times");
					break;
				case '/':
					writeStatic(writer, "_div");
					break;
				case '<':
					writeStatic(writer, "_less");
					break;
				case '>':
					writeStatic(writer, "_greater");
					break;
				case '=':
					writeStatic(writer, "_equal");
					break;
				default:
					unreachable<void>();
			}
		});
	} else {
		if (conflictsWithCName(name))
			writeChar(writer, '_');
		eachCharInSym(name, [&](const char c) {
			switch (c){
				case '-':
					writeChar(writer, '_');
					break;
				case '?':
					writeStatic(writer, "__q");
					break;
				default:
					writeChar(writer, c);
					break;
			}
		});
	}
}

const Str mangleExternFunName(Arena* arena, const Sym name) {
	Arena temp {};
	return map<const char>{}(
		arena,
		strOfSym(&temp, name),
		[](const char c) {
			if (isMangledChar(c))
				return c;
			else if (c == '-')
				return '_';
			else
				return todo<const char>("extern fun with unusual char?");
		});
}

const Bool isMangledName(const Str name) {
	return every(name, isMangledChar);
}
