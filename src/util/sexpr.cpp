#include "./sexpr.h"

Output& operator<<(Output& out, const Sexpr s) {
	s.match(
		[&](const Arr<const Sexpr> s) {
			out << "[";
			writeWithCommas(out, s);
			out << "]";
		},
		[&](const SexprRecord s) {
			out << s.name;
			out << "(";
			writeWithCommas(out, s.children);
			out << ")";
		},
		[&](const Str s) {
			out << s;
		});
	return out;
}
