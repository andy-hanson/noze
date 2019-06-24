#pragma once

#include "../util.h"

struct Output {
	Output& operator<<(const Str s);
	Output& operator<<(const size_t s);
	Output& operator<<(const Int64 i);

	inline Output& operator<<(const uint u) {
		return *this << static_cast<size_t>(u);
	}

	inline Output& operator<<(const int i) {
		return *this << static_cast<Int64>(i);
	}

	Output& operator<<(const CStr c);
	Output& operator<<(const char c);
	Output& operator<<(const Bool b);
};

template <typename T>
void writeWithCommas(Output& out, const Arr<T> a) {
	for (const size_t i : Range{a.size}) {
		if (i != 0)
			out << ", ";
		out << a[i];
	}
}

void writeEscapedChar(Output& out, const char c);
