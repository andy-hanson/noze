#include "./writer.h"

void Writer::writeStr(const Str s) {
	if (!s.isEmpty()) {
		writeChar(s[0]);
		writeStr(tail(s));
	}
}

void Writer::writeInt(const ssize_t s) {
	if (s < 0)
		writeChar('-');
	writeUint(s < 0 ? -s : s);
}

void Writer::writeUint(const size_t s) {
	if (s >= 10)
		writeUint(s / 10);
	writeChar('0' + s % 10);
}
