#include "./sexpr.h"

void writeSexpr(Writer& writer, const Sexpr s) {
	s.match(
		[&](const Arr<const Sexpr> s) {
			writeChar(writer, '[');
			writeWithCommas(writer, s, [&](const Sexpr element) {
				writeSexpr(writer, element);
			});
			writeChar(writer, ']');
		},
		[&](const SexprRecord s) {
			writeSym(writer, s.name);
			writeChar(writer, '(');
			writeWithCommas(writer, s.children, [&](const Sexpr element) {
				writeSexpr(writer, element);
			});
			writeChar(writer, ')');
		},
		[&](const Str s) {
			//TODO: quoted
			writeStr(writer, s);
		},
		[&](const Sym s) {
			writeSym(writer, s);
		});
}
