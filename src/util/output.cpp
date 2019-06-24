#include "./output.h"

Output& Output::operator<<(const Str s) {
	printf("%.*s", safeSizeTToInt(s.size), s.begin());
	return *this;
}

Output& Output::operator<<(const size_t s) {
	printf("%zu", s);
	return *this;
}

Output& Output::operator<<(const Int64 i) {
	printf("%zd", i);
	return *this;
}

Output& Output::operator<<(const CStr c) {
	printf("%s", c);
	return *this;
}

Output& Output::operator<<(const char c) {
	printf("%c", c);
	return *this;
}

Output& Output::operator<<(const Bool b) {
	return *this << (b ? "true" : "false");
}

void writeEscapedChar(Output& out, const char c) {
	switch (c) {
		case '\n':
			out << "\\n";
			break;
		case '\t':
			out << "\\t";
			break;
		case '\'':
			out << "\\\'";
			break;
		case '"':
			out << "\\\"";
			break;
		case '\\':
			out << "\\\\";
			break;
		case '\0':
			out << "\\0";
			break;
		default:
			out << c;
			break;
	}
}
