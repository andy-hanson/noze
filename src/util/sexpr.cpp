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
			writeStr(writer, s.name);
			writeChar(writer, '(');
			writeWithCommas(writer, s.children, [&](const Sexpr element) {
				writeSexpr(writer, element);
			});
			writeChar(writer, ')');
		},
		[&](const Str s) {
			writeStr(writer, s);
		});
}
